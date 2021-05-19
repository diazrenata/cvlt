#' Subset an LDA model
#'
#' Takes an LDA model fit to an ENTIRE dataset and extracts just rows present in the TRAIN data for a subsetted data item.
#'
#' Subsets both the topic proportions  and loglikelihood, to maintain the correct dimensions.
#'
#' @param fitted_lda LDA fit to FULL dataset
#' @param subsetted_dataset_item result of subset_data_one, list with `$test` and `$train` elements.
#'
#' @return fitted_lda subset to just the timesteps present in the `$train` element of `subsetted_dataset_item`.
#' @export
#'
subset_lda <- function(fitted_lda, subsetted_dataset_item) {

  keep_rows <- subsetted_dataset_item$full$covariates$year %in% subsetted_dataset_item$train$covariates$year

  fitted_lda@gamma <- fitted_lda@gamma[ which(keep_rows), ]

  fitted_lda@wordassignments <- NULL

  fitted_lda@Dim[1] <- sum(keep_rows)

  fitted_lda@loglikelihood <- fitted_lda@loglikelihood[ which(keep_rows)]

  return(fitted_lda)
}

#' Get abundance probabilities
#'
#' Get probabilities of abundances, as the multinomial probabilities, for each species at each time step as predicted by a TS model.
#'
#' Gets one matrix of probabiltiies for each draw from the posterior for estimates of changepoint locations and model parameters (in this case just the intercept).
#'
#' @param subsetted_dataset_item list with (at least) element `$full`: unaltered dataset.
#' @param fitted_lda LDA fit to either a subsetted or a full dataset
#' @param fitted_ts TS fit to that LDA
#' @param max_sims defaults NULL. If supplied, will randomly draw `max_sims` draws from the posterior and return probabilities just for those. Useful for speeding things up.
#'
#' @return list of matrices of abundance probabilities for each species (columns) for each timestep (rows), with one list for each draw from the posterior (or `max_sims`, if supplied).
#' @export
#'
get_abund_probabilities <- function(
  subsetted_dataset_item,
  fitted_lda,
  fitted_ts,
  max_sims = NULL
) {

  betas <- exp(fitted_lda@beta)

  nsims = nrow(fitted_ts$etas)

  these_sims <- 1:nsims

  if(!is.null(max_sims)) {

    if(nsims > max_sims) {

      these_sims <- sample.int(nsims, max_sims, FALSE)

    }
  }


  # The `theta` matrix is the matrix of predicted topic proportions for a given draw from the posterior.
  thetas <- lapply(these_sims, FUN = multinom_theta, subsetted_dataset_item = subsetted_dataset_item, ts_model = fitted_ts)

  # The predicted *abundances* is the product of the theta and beta matrices.
  abund_probabilities <- lapply(thetas, FUN = function(theta, betas) return(theta %*% betas), betas = betas)

  # Remove matrices that result in NAs. RMD check what case caused you to add this.
  na_probs <- vector()
  for(i in 1:length(abund_probabilities)) {
    if(anyNA(abund_probabilities[[i]])) {
      na_probs <- c(na_probs, i)
    }
  }

  if(length(na_probs) > 0) {
    abund_probabilities <- abund_probabilities[-na_probs]
  }
  return(abund_probabilities)
}

#' Get theta matrix for one draw from posterior
#'
#' The theta matrix is the predicted topic proportions.
#'
#' @param subsetted_dataset_item list with (at least) element `$full`: unaltered dataset.
#' @param ts_model TS fit to either the full dataset or a subset
#' @param sim integer, which draw from the posterior to extract the thetas for
#'
#' @return matrix of predicted topic proportions (columns) for each timestep in the full dataset (rows)
#' @export
#'
#' @importFrom dplyr filter select mutate distinct bind_rows left_join
#' @importFrom LDATS multinom_TS
multinom_theta <- function (subsetted_dataset_item, ts_model, sim = 1)
{

  rho <- ts_model$rhos[sim, ]

  x <- ts_model

  seg_mods <- LDATS::multinom_TS(x$data, x$formula, rho, x$timename,
                                 x$weights, x$control)
  nsegs <- length(seg_mods[[1]])


  # Get all possible years - including possible skipped years

  full_timespan <- min(subsetted_dataset_item$full$covariates$year):
    max(subsetted_dataset_item$full$covariates$year)

  full_segs <- data.frame(year = full_timespan,
                          segment = NA)

  # Break up the full possible timespan into segments (before and after each rho)
  for(i in 1:nsegs) {
    if(nsegs == 1) {
      these_boundaries <- full_timespan
    } else if(i == 1) {
      these_boundaries <-
        min(full_timespan):
        rho[1]
    } else if (i == nsegs) {
      these_boundaries <- (rho[i - 1]+1):max(full_timespan)
    } else {
      these_boundaries <- (rho[i - 1] + 1):rho[i]
    }

    full_segs$segment[ which(full_segs$year %in% these_boundaries)] <- i
  }

  segment_estimates <- list()
  #fitted_values <- list()

  # For each segment, use the segment-level models to predict the topic proportions for that segment
  for(i in 1:nsegs) {

    this_seg <- seg_mods[[1]][i]

    this_fit <- as.data.frame(this_seg[[1]]$fitted.values)
    #these_years <- (this_seg[[1]]$timevals)

    #this_fit <- cbind(this_fit, year = these_years)

   # fitted_values[[i]] <- this_fit

    segment_estimates[[i]] <- this_fit %>%
      dplyr::mutate(segment = i) %>%
     # dplyr::select(-year) %>%
      dplyr::distinct()
  }

 # fitted_values <- dplyr::bind_rows(fitted_values)
  segment_estimates <- dplyr::bind_rows(segment_estimates)

  # Add these predictions to the full timeseries
  predicted_values <- dplyr::left_join(full_segs, segment_estimates) %>%
    select(-segment)

  # Filter these predictions to just the years that occur in the actual data
  Theta <- dplyr::filter(predicted_values, year %in% subsetted_dataset_item$full$covariates$year)

  Theta <- dplyr::select(Theta, -year)

  Theta <- as.matrix(Theta)

  Theta
}

#' Get test loglikelihood (all)
#'
#' Wrapper for `get_one_test_loglik.` Gets loglikelihood estimate of the test timestep for every draw from the posterior (or every draw supplied via `abund_probabilities`).
#'
#' @param subsetted_dataset_item list with - at least - elements `test` (of abundance matrix and covaraites for test timestep) and `test_timestep` (the row number, of the full dataset, of the test timestep).
#' @param abund_probabilities list of abund_probabilities; with an element for every draw from the posterior being considered.
#'
#' @return vector of loglikelihood of test data given every abund_probability estimate
#' @export
#'
get_test_loglik <- function(
  subsetted_dataset_item,
  abund_probabilities
) {

  test_logliks <- lapply(abund_probabilities, FUN = get_one_test_loglik, subsetted_dataset_item = subsetted_dataset_item)

  return(unlist(test_logliks))

}


#' Get test row logliklihood (for one draw)
#'
#' Get loglikelihood of observed abundances for test timestep given predicted abundance probabilities, for a single draw from the posterior.
#'
#' @param subsetted_dataset_item list with - at least - elements `test` (of abundance matrix and covaraites for test timestep) and `test_timestep` (the row number, of the full dataset, of the test timestep).
#' @param abund_probabilities_one ONE matrix of abundance probabilities
#'
#' @return numeric, loglikelihood of the observed abundances from the test timestep given abund_probabilities
#' @export
#'
get_one_test_loglik <- function(
  subsetted_dataset_item,
  abund_probabilities_one
) {

  test_dat <- subsetted_dataset_item$test$abundance

  test_row_number <- subsetted_dataset_item$test_timestep

  test_logliks <- vector()

  for(i in 1:length(test_row_number)) {
    test_logliks <- c(test_logliks, dmultinom(x = test_dat[i, ], prob = abund_probabilities_one[test_row_number[i],], log = TRUE))
  }

  test_loglik <- sum(test_logliks)

  return(test_loglik)
}
