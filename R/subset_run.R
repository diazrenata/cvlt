#' Run LDATS on a single dataset subset
#'
#' This function runs on a single subset (e.g. the dataset with timestep 1 as the test timestep). Run `fit_ldats_crossval` to run this function on every subset.
#'
#' First, fits an LDA to the *full* (not subsetted) dataset. Then splits the matrix of topic proportions (`gamma` matrix) for that LDA into training/test subsets to match the subset. (The LDA is fit to the full dataset, because LDAs fit to different subsets cannot be recombined in a logical way).
#'
#' Then fits a TS model to the *subsetted* `gamma` matrix, with the specified number of iterations & changepoints.
#'
#' Then extracts from that TS model the predicted abundances (multinomial probability distribution of species abundances) for each timestep. Because of the Bayesian component of the changepoint model, there is a matrix of predicted abundances per timestep *for every draw from the posterior*, so `nit` matrices. Then calculates the loglikelihood of the test timestep given these predicted probabilities. There are `nit` estimates of the loglikelihood.
#'
#' Returns the subsetted dataset item list provided, with the following appendend: The LDA, TS, and abundance probabilities (if `return_full = TRUE`), or as NULL otherwise; the vector of loglikelihoods for the test timestep for each iteration; a list `model_info` with the model specifications `(k, seed, cpts, nit)`
#'
#'
#'
#' @param subsetted_dataset_item Result of subset_data_one, list with elements `$full`, `$train`, `$test`, `$test_timestep`
#' @param k integer Number of topics for the LDA model.
#' @param seed integer Seed for running LDA model. Only use even numbers (odd numbers duplicate adjacent evens).
#' @param cpts integer How many changepoints for ts?
#' @param nit integer How many iterations? (draws from posterior)
#' @param return_full logical Whether to return fitted model objects and abundance probabilities in addition to logliks. Can be useful for diagnostics, but hogs memory. Default FALSE.
#'
#' @return list. subsetted_dataset_item with the following appended: If `return_full`, fitted_lda; fitted_ts; abund_probabilities, otherwise NULL; test_logliks, model_info
#' @export
#'
#' @importFrom LDATS TS_on_LDA TS_control
ldats_subset_one <- function(subsetted_dataset_item,
                             k,
                             seed,
                             cpts,
                             nit,
                             return_full = FALSE) {


  # Fit LDA with `k` topics and `seed` to the FULL abundance timeseries
  fitted_lda <- LDA_set_user_seeds(
    document_term_table = subsetted_dataset_item$full$abundance,
    topics = k,
    seed = seed)[[1]]

  # Subset the gammas and loglikelihoods for that LDA to match the train/test split for this subset
  subsetted_lda <- subset_lda(fitted_lda, subsetted_dataset_item)

  # Fit TS model with `cpts` and `nit` to the subsetted gammas
  fitted_ts <- LDATS::TS_on_LDA(subsetted_lda,
                                document_covariate_table = as.data.frame(subsetted_dataset_item$train$covariates),
                                timename = "year",
                                formulas = ~1,
                                nchangepoints = cpts,
                                control = LDATS::TS_control(nit = nit))[[1]]

  # Extract predicted multinomial predictions for all years and all draws from posterior
  abund_probabilities <- get_abund_probabilities(
    subsetted_dataset_item,
    subsetted_lda,
    fitted_ts
  )

  # Calculate loglikelihood of test timestep for each draw from the posterior
  test_logliks  <- get_test_loglik(
    subsetted_dataset_item,
    abund_probabilities
  )

  if(return_full) {

  subsetted_dataset_item$fitted_lda <- subsetted_lda
  subsetted_dataset_item$fitted_ts <- fitted_ts
  subsetted_dataset_item$abund_probabilities <- abund_probabilities

  } else {

    subsetted_dataset_item$fitted_lda <- NULL
    subsetted_dataset_item$fitted_ts <- NULL
    subsetted_dataset_item$abund_probabilities <- NULL
  }

    subsetted_dataset_item$test_logliks <- test_logliks

    subsetted_dataset_item$model_info <- list(k = k, seed = seed, cpts = cpts, nit = nit)

  return(subsetted_dataset_item)

}
