#' #' Subset an LDA model
#' #'
#' #' Takes an LDA model fit to an ENTIRE dataset and strips it down to the rows present in the TRAIN data for a subsetted data item
#' #'
#' #' @param fitted_lda lda fit to FULL dataset
#' #' @param subsetted_dataset_item result of subset_data_one, test and train
#' #'
#' #' @return fitted_lda cut and pasted to be just the rows present in the train data
#' #' @export
#' #'
#' subset_lda <- function(fitted_lda, subsetted_dataset_item) {
#'
#'   keep_rows <- subsetted_dataset_item$full$covariates$year %in% subsetted_dataset_item$train$covariates$year
#'
#'   fitted_lda@gamma <- fitted_lda@gamma[ which(keep_rows), ]
#'
#'   fitted_lda@wordassignments <- NULL
#'
#'   fitted_lda@Dim[1] <- sum(keep_rows)
#'
#'   fitted_lda@loglikelihood <- fitted_lda@loglikelihood[ which(keep_rows)]
#'
#'   return(fitted_lda)
#' }
#' #' Get abundance probabilities
#' #'
#' #' Get probabilities of abundances for each species at each time step as predicted by a TS model.
#' #'
#' #' Gets one matrix of probabiltiies for each draw from the posterior for estimates of changepoint locations and model parameters (in this case just the intercept)
#' #'
#' #' @param subsetted_dataset_item dat
#' #' @param fitted_lda fitted lda
#' #' @param fitted_ts fitted ts
#' #'
#' #' @return list of abund prob matrices for each draw from posterior
#' #' @export
#' #'
#' get_abund_probabilities <- function(
#'   subsetted_dataset_item,
#'   fitted_lda,
#'   fitted_ts,
#'   max_sims = NULL
#' ) {
#'
#'   betas <- exp(fitted_lda@beta)
#'
#'   nsims = nrow(fitted_ts$etas)
#'
#'   these_sims <- 1:nsims
#'
#'   if(!is.null(max_sims)) {
#'
#'     if(nsims > max_sims) {
#'
#'       these_sims <- sample.int(nsims, max_sims, FALSE)
#'
#'     }
#'   }
#'
#'
#'   thetas <- lapply(these_sims, FUN = get_one_mn_theta, subsetted_dataset_item = subsetted_dataset_item, fitted_ts = fitted_ts)
#'
#'   abund_probabilities <- lapply(thetas, FUN = function(theta, betas) return(theta %*% betas), betas = betas)
#'
#'   na_probs <- vector()
#'   for(i in 1:length(abund_probabilities)) {
#'     if(anyNA(abund_probabilities[[i]])) {
#'       na_probs <- c(na_probs, i)
#'     }
#'   }
#'
#'   if(length(na_probs) > 0) {
#'     abund_probabilities <- abund_probabilities[-na_probs]
#'   }
#'   return(abund_probabilities)
#' }
#'
#' get_one_mn_theta <- function(subsetted_dataset_item,
#'                              fitted_ts,
#'                              sim = 1) {
#'
#'
#'   Theta <- multinom_theta(subsetted_dataset_item, fitted_ts, sim)
#'
#'   Theta <- dplyr::filter(Theta, year %in% subsetted_dataset_item$full$covariates$year)
#'
#'   Theta <- dplyr::select(Theta, -year)
#'
#'   Theta <- as.matrix(Theta)
#'
#'   # Theta <- Theta [ which(year %in% time_span),]
#'
#'   return(Theta)
#' }
#'
#' multinom_theta <- function (subsetted_dataset_item, ts_model, sim = 1)
#' {
#'
#'   rho <- ts_model$rhos[sim, ]
#'
#'   x <- ts_model
#'
#'   seg_mods <- LDATS::multinom_TS(x$data, x$formula, rho, x$timename,
#'                                  x$weights, x$control)
#'   nsegs <- length(seg_mods[[1]])
#'
#'
#'   full_timespan <- min(subsetted_dataset_item$full$covariates$year):
#'     max(subsetted_dataset_item$full$covariates$year)
#'
#'   full_segs <- data.frame(year = full_timespan,
#'                           segment = NA)
#'
#'   for(i in 1:nsegs) {
#'     if(nsegs == 1) {
#'       these_boundaries <- full_timespan
#'     } else if(i == 1) {
#'       these_boundaries <-
#'         min(full_timespan):
#'         rho[1]
#'     } else if (i == nsegs) {
#'       these_boundaries <- (rho[i - 1]+1):max(full_timespan)
#'     } else {
#'       these_boundaries <- (rho[i - 1] + 1):rho[i]
#'     }
#'
#'     full_segs$segment[ which(full_segs$year %in% these_boundaries)] <- i
#'   }
#'
#'   segment_estimates <- list()
#'   fitted_values <- list()
#'
#'   for(i in 1:nsegs) {
#'
#'     this_seg <- seg_mods[[1]][i]
#'
#'     this_fit <- as.data.frame(this_seg[[1]]$fitted.values)
#'     these_years <- (this_seg[[1]]$timevals)
#'
#'     this_fit <- cbind(this_fit, year = these_years)
#'
#'     fitted_values[[i]] <- this_fit
#'
#'     segment_estimates[[i]] <- this_fit %>%
#'       dplyr::mutate(segment = i) %>%
#'       dplyr::select(-year) %>%
#'       dplyr::distinct()
#'   }
#'
#'   fitted_values <- dplyr::bind_rows(fitted_values)
#'   segment_estimates <- dplyr::bind_rows(segment_estimates)
#'
#'   predicted_values <- dplyr::left_join(full_segs, segment_estimates) %>%
#'     select(-segment)
#'
#'   predicted_values
#' }
#'
#' #' Get test loglikelihood (all)
#' #'
#' #' Wrapper for get_one_test_loglik. Gets loglikelihood estimate for every draw from the posterior.
#' #'
#' #' @param subsetted_dataset_item result of subset_data_one
#' #' @param abund_probabilities list of abund_probabilities; one element for every draw from posterior
#' #'
#' #' @return vector of loglikelihood of test data given every abund_probability estimate
#' #' @export
#' #'
#' get_test_loglik <- function(
#'   subsetted_dataset_item,
#'   abund_probabilities
#' ) {
#'
#'   test_logliks <- lapply(abund_probabilities, FUN = get_one_test_loglik, subsetted_dataset_item = subsetted_dataset_item)
#'
#'   return(unlist(test_logliks))
#'
#' }
#'
#'
#' #' Get test row logliklihood (for one draw)
#' #'
#' #' Get loglikelihood of observed abundances for test row given abundance probabilties. For one draw from the posterior
#' #'
#' #' @param subsetted_dataset_item result of subset_data_one
#' #' @param abund_probabilities_one ONE matrix of abundance probabilities
#' #'
#' #' @return loglikelihood of obs abundances in test row given abund_probabilities
#' #' @export
#' #'
#' get_one_test_loglik <- function(
#'   subsetted_dataset_item,
#'   abund_probabilities_one
#' ) {
#'
#'   test_dat <- subsetted_dataset_item$test$abundance
#'
#'   test_row_number <- subsetted_dataset_item$test_timestep
#'
#'   test_logliks <- vector()
#'
#'   for(i in 1:length(test_row_number)) {
#'     test_logliks <- c(test_logliks, dmultinom(x = test_dat[i, ], prob = abund_probabilities_one[test_row_number[i],], log = TRUE))
#'   }
#'
#'   test_loglik <- sum(test_logliks)
#'
#'   return(test_loglik)
#' }
