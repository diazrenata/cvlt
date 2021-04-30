#' #' Run LDATs on a single subsetted dataset
#' #'
#' #' This is the big wrapper function.
#' #'
#' #' Fits an LDA to a subsetted dataset item.
#' #'
#' #' IF fit_to_train, fits LDA to the train data. IF fit_to_train == FALSE, fits LDA to FULL dataset.
#' #'
#' #' IF fit_to_train == FALSE, then SUBSETS the lda to contain only the gammas (and loglikelihoods) for the timesteps in the TRAIN dataset.
#' #'
#' #' Fits a TS model with specified ncpts, nit
#' #'
#' #' Extracts from TS, predicted abundances (as multinom probability distribution) for each species at each timestep. There is a matrix of abundance probabilities *for every draw in the posterior, so nit*
#' #'
#' #' Calculates the *loglikelihood of the test row* given abundance probabiltiies. There is a test loglikelihood for every draw in the posterior, so nit.
#' #'
#' #'
#' #'
#' #' @param subsetted_dataset_item result of subset_data_one
#' #' @param k ntopics for lda
#' #' @param seed seed for lda. only use even numbers.
#' #' @param cpts how many changepoints for ts?
#' #' @param nit how many iterations? (draws from posterior)
#' #' @param fit_to_train fit LDA to TRAINING DATA ONLY (default) or set to FALSE to fit to ALL DATA and then subset
#' #'
#' #' @return list. subsetted_dataset_item with the following appended. fitted_lda; fitted_ts; abund_probabilities; test_logliks, model_info
#' #' @export
#' #'
#' ldats_subset_one <- function(subsetted_dataset_item,
#'                              k,
#'                              seed,
#'                              cpts,
#'                              nit,
#'                              fit_to_train = TRUE) {
#'
#'   if(fit_to_train) {
#'
#'     fitted_lda <- LDATS::LDA_set_user_seeds(
#'       document_term_table = subsetted_dataset_item$train$abundance,
#'       topics = k,
#'       seed = seed)[[1]]
#'
#'   }  else {
#'
#'     fitted_lda <- LDATS::LDA_set_user_seeds(
#'       document_term_table = subsetted_dataset_item$full$abundance,
#'       topics = k,
#'       seed = seed)[[1]]
#'
#'     fitted_lda <- subset_lda(fitted_lda, subsetted_dataset_item)
#'
#'   }
#'
#'   fitted_ts <- LDATS::TS_on_LDA(fitted_lda,
#'                          document_covariate_table = as.data.frame(subsetted_dataset_item$train$covariates),
#'                          timename = "year",
#'                          formulas = ~1,
#'                          nchangepoints = cpts,
#'                          control = LDATS::TS_control(nit = nit))[[1]]
#'
#'   abund_probabilities <- get_abund_probabilities(
#'     subsetted_dataset_item,
#'     fitted_lda,
#'     fitted_ts
#'   )
#'
#'   test_logliks  <- get_test_loglik(
#'     subsetted_dataset_item,
#'     abund_probabilities
#'   )
#'
#'   subsetted_dataset_item$fitted_lda <- fitted_lda
#'   subsetted_dataset_item$fitted_ts <- fitted_ts
#'   subsetted_dataset_item$abund_probabilities <- abund_probabilities
#'   subsetted_dataset_item$test_logliks <- test_logliks
#'   subsetted_dataset_item$model_info <- list(k = k, seed = seed, cpts = cpts, nit = nit)
#'
#'   return(subsetted_dataset_item)
#'
#' }
