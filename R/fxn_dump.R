#' #' Subset data - all subsets
#' #'
#' #' Wrapper for `subset_data_one` to create a list of all data subsets using 1 timestep of test data per subset and a buffer on either side
#' #'
#' #' @param full_dataset MATSS-style dataset. A list with elements `$abundance`, `$covariates`
#' #' @param buffer_size number of timesteps to withold on either side of the test timestep. Defaults 2
#' #'
#' #' @return a list of ntimesteps lists, if ntimesteps is the number of timesteps in `full_dataset`. each list in the list is the output of `subset_data_one` with the test timestep as one of the timesteps in the full dataset.
#' #' @export
#' #'
#' subset_data_all <- function(full_dataset, use_folds = FALSE, n_timesteps = 2, n_folds = 5, buffer_size = 2) {
#'
#'   if(!use_folds) {
#'     subsetted_data = lapply(1:nrow(full_dataset$abundance), FUN = subset_data_one, full_dataset = full_dataset, buffer_size = buffer_size)
#'   }  else{
#'     subsetted_data = replicate(n = n_folds, expr = subset_data_one_folds(full_dataset = full_dataset, n_timesteps = n_timesteps, buffer_size = buffer_size), simplify = F)
#'   }
#'
#'   return(subsetted_data)
#' }
#'
#' #' Subset data - one subset
#' #'
#' #' Creates a *single* training/test subset for a dataset.
#' #' Witholds a single timestep for testing, and a buffer of timesteps around that timestep.
#' #'
#' #' @param full_dataset MATSS style dataset. A list with elements `$abundance`, `$covariates`
#' #' @param test_timestep Which timestep (row) to withold and use for test
#' #' @param buffer_size How many rows to withold on either side (not used for test)
#' #'
#' #' @return list with elements train (list of $abundance, $covariates), test (list of $abundance, $covariates), full (unaltered full_dataset), test_timestep (which tstep is the test one), buffer_size (buffer size)
#' #' @export
#' #'
#' subset_data_one <- function(full_dataset, test_timestep, buffer_size) {
#'
#'   timesteps_to_withold <- c((test_timestep - buffer_size):(test_timestep + buffer_size))
#'   timesteps_to_withold <- timesteps_to_withold[
#'     which(timesteps_to_withold %in% 1:nrow(full_dataset$abundance))]
#'
#'   timesteps_to_keep <- 1:nrow(full_dataset$abundance)
#'   timesteps_to_keep <- timesteps_to_keep[ which(!(timesteps_to_keep %in% timesteps_to_withold  ))]
#'
#'   test_data <- list(
#'     abundance = full_dataset$abundance[ test_timestep, ],
#'     covariates = full_dataset$covariates[ test_timestep, ]
#'   )
#'
#'   train_data <- list(
#'     abundance = full_dataset$abundance[ timesteps_to_keep, ],
#'     covariates = full_dataset$covariates[ timesteps_to_keep, ])
#'
#'   subsetted_dataset <- list(
#'     train = train_data,
#'     test = test_data,
#'     full = full_dataset,
#'     test_timestep = test_timestep,
#'     buffer_size = buffer_size
#'   )
#'
#'   return(subsetted_dataset)
#'
#' }
#'
#' #' Subset data - one subset
#' #'
#' #' Creates a *single* training/test subset for a dataset.
#' #' Witholds a single timestep for testing, and a buffer of timesteps around that timestep.
#' #'
#' #' @param full_dataset MATSS style dataset. A list with elements `$abundance`, `$covariates`
#' #' @param test_timestep Which timestep (row) to withold and use for test
#' #' @param buffer_size How many rows to withold on either side (not used for test)
#' #'
#' #' @return list with elements train (list of $abundance, $covariates), test (list of $abundance, $covariates), full (unaltered full_dataset), test_timestep (which tstep is the test one), buffer_size (buffer size)
#' #' @export
#' #'
#' subset_data_one_folds <- function(full_dataset, n_timesteps, buffer_size) {
#'   test_timesteps <- vector()
#'
#'   available_timesteps <- 1:nrow(full_dataset$abundance)
#'   last_timestep <- nrow(full_dataset$abundance)
#'
#'   ntries <- 0
#'
#'   while(all(length(test_timesteps) < n_timesteps ,ntries < 100)){
#'     candidate <- sample(available_timesteps, 1)
#'
#'     candidate_buffer <- c((candidate - buffer_size):(candidate + buffer_size))
#'
#'     candidate_buffer <- candidate_buffer[ which(candidate_buffer %in% 1:last_timestep)]
#'
#'     if(all(candidate_buffer %in% available_timesteps)) {
#'       available_timesteps <- available_timesteps[ which(!(available_timesteps %in% candidate_buffer))]
#'       test_timesteps <- c(test_timesteps, candidate)
#'     }
#'
#'     ntries <- ntries + 1
#'   }
#'
#'   stopifnot(length(test_timesteps) == n_timesteps)
#'
#'   timesteps_to_withold <- vector()
#'   for(i in 1:length(test_timesteps)) {
#'     timesteps_to_withold <- c(timesteps_to_withold, c((test_timesteps[i] - buffer_size):(test_timesteps[i] + buffer_size)))
#'   }
#'
#'   timesteps_to_withold <- timesteps_to_withold[
#'     which(timesteps_to_withold %in% 1:nrow(full_dataset$abundance))]
#'
#'   timesteps_to_keep <- 1:nrow(full_dataset$abundance)
#'   timesteps_to_keep <- timesteps_to_keep[ which(!(timesteps_to_keep %in% timesteps_to_withold  ))]
#'
#'   test_data <- list(
#'     abundance = full_dataset$abundance[ test_timesteps, ],
#'     covariates = full_dataset$covariates[ test_timesteps, ]
#'   )
#'
#'   train_data <- list(
#'     abundance = full_dataset$abundance[ timesteps_to_keep, ],
#'     covariates = full_dataset$covariates[ timesteps_to_keep, ])
#'
#'   subsetted_dataset <- list(
#'     train = train_data,
#'     test = test_data,
#'     full = full_dataset,
#'     test_timestep = test_timesteps,
#'     buffer_size = buffer_size
#'   )
#'
#'   return(subsetted_dataset)
#'
#' }
#'
#'
#' make_long_fold_loglik_df <- function(a_fold) {
#'
#'   df <- data.frame(
#'     test_steps = toString(a_fold$test_timestep),
#'     ntests = length(a_fold$test_timestep),
#'     log_lik = a_fold$test_logliks
#'   )
#'
#'   df
#' }
#'
#'
#' make_long_folds_loglik_df <- function(all_folds) {
#'
#'   dfs <- lapply(all_folds, make_long_fold_loglik_df)
#'
#'   dfs <- bind_rows(dfs)
#'
#'   ll_df <- as.data.frame(all_folds[[1]]$model_info)
#'
#'   ll_df$nfolds <- length(all_folds)
#'
#'   cbind(ll_df, dfs)
#'
#' }
#'
#'
#' estimate_fold_loglik <- function(one_fold, summary = "mean") {
#'   if(summary == "mean") {
#'     return(mean(one_fold$test_logliks))
#'   }
#' }
#'
#' make_folds_loglik_df <- function(all_folds) {
#'
#'   ll_df <- as.data.frame(all_folds[[1]]$model_info)
#'
#'   ll_df$nfolds <- length(all_folds)
#'
#'   sum_loglik <- vapply(all_folds, estimate_fold_loglik, FUN.VALUE = 100)
#'
#'   ntests <- vapply(all_folds, FUN = function(a_fold) return(length(a_fold$test_timestep)), FUN.VALUE = 2)
#'
#'   test_steps <- vapply(all_folds, FUN = function(a_fold) return(toString(a_fold$test_timestep)), FUN.VALUE = "1, 2")
#'
#'   cbind(ll_df, sum_loglik, ntests, test_steps)
#' }
#'
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
#'
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
#'     fitted_lda <- LDA_set_user_seeds(
#'       document_term_table = subsetted_dataset_item$train$abundance,
#'       topics = k,
#'       seed = seed)[[1]]
#'
#'   }  else {
#'
#'     fitted_lda <- LDA_set_user_seeds(
#'       document_term_table = subsetted_dataset_item$full$abundance,
#'       topics = k,
#'       seed = seed)[[1]]
#'
#'     fitted_lda <- subset_lda(fitted_lda, subsetted_dataset_item)
#'
#'   }
#'
#'   fitted_ts <- TS_on_LDA(fitted_lda,
#'                          document_covariate_table = as.data.frame(subsetted_dataset_item$train$covariates),
#'                          timename = "year",
#'                          formulas = ~1,
#'                          nchangepoints = cpts,
#'                          control = TS_control(nit = nit))[[1]]
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
#'
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
#'
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
#' #' #' Get one theta matrix
#' #' #'
#' #' #' The theta matrix is the predicted proportions of each LDA topic at each time step. There is one theta matrix for every estimate of Eta (model pars) and rho (changepoint locations). This gets the theta matrix for *one* estimate.
#' #' #'
#' #' #' @param subsetted_dataset_item dat
#' #' #' @param fitted_ts fitted ts to get thetas from
#' #' #' @param sim which draw from posterior
#' #' #'
#' #' #' @return matrix of predicted proportions for each LDA topic at each timestep
#' #' #' @export
#' #' #'
#' #' get_one_theta <- function(subsetted_dataset_item,
#' #'                           fitted_ts,
#' #'                           sim = 1) {
#' #'   covars <- subsetted_dataset_item$full$covariates$year
#' #'
#' #'   ncovar <- 1
#' #'
#' #'   nseg <- ifelse(is.null(fitted_ts$rhos), 1, ncol(fitted_ts$rhos) + 1)
#' #'
#' #'   ntopics <- ncol(fitted_ts$data$gamma)
#' #'
#' #'   ndocs <- nrow(subsetted_dataset_item$full$covariates)
#' #'
#' #'   time_span <- min(subsetted_dataset_item$full$covariates$year):max(subsetted_dataset_item$full$covariates$year)
#' #'
#' #'   time_span_length <- length(time_span)
#' #'
#' #'   X <- matrix(nrow = time_span_length, ncol = ncovar, data = time_span, byrow = FALSE)
#' #'
#' #'   # model_Eta <- fitted_ts$etas[sim, ]
#' #'   #
#' #'   # Eta_matrix <- matrix(nrow = ncovar  * nseg, ncol = ntopics,
#' #'   #                      data = c(rep(0, times = ncovar * nseg), model_Eta), byrow = FALSE)
#' #'   #
#' #'   rho = fitted_ts$rhos[sim,]
#' #'
#' #'   tD <- unlist(subsetted_dataset_item$full$covariates$year)
#' #'
#' #'   Theta <- multinom_theta(X, rho, time_span)
#' #'
#' #'   Theta <- select(Theta, -year)
#' #'
#' #'   Theta <- as.matrix(Theta)
#' #'   #Theta <- softmax(Theta)
#' #'
#' #'   Theta <- Theta [ which(tD %in% time_span),]
#' #'
#' #'   return(Theta)
#' #' }
#' #'
#' #'
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
#'
#' #' Stitch together aggregate loglik over a TS
#' #'
#' #' Given a list of length ntimesteps,
#' #'
#' #' where each element in the list is the vector of length ndraws of test logliklihoods for a model fit to a train/test subset focused on a single timestep,
#' #'
#' #' stitch together a ts of loglikelihoods by randomly drawing one loglikelihood for each time step.
#' #'
#' #' @param many_fits list described above
#' #'
#' #' @return summed loglik across entire ts
#' #' @export
#' #'
#' compose_ts_loglik <- function(many_fits) {
#'
#'   # nsims <- many_fits[[1]]$model_info$nit
#'
#'   ts_logliks <- sum(unlist(lapply(many_fits, FUN = function(fits) return(fits$test_logliks[ sample.int(n = length(fits$test_logliks), size = 1)]))))
#'
#' }
#'
#' #' Repeatedly estimate loglik for a ts fit
#' #'
#' #' Wrapper for compose_ts_loglik to get many estimates fo the loglikelihood for a TS fit.
#' #'
#' #' @param many_fits list, one element per timestep, of loglik estimates for the ts model fit with that timestep as the test timestep
#' #' @param nests how many estimates to generate
#' #'
#' #' @return vector of nests estimated aggregate logliks
#' estimate_ts_loglik <- function(many_fits, nests) {
#'
#'   return(list(
#'     model_info = many_fits[[1]]$model_info,
#'     loglik_ests = replicate(n = nests, compose_ts_loglik(many_fits), simplify = T)))
#' }
#'
#'
#'
#' #' Bundle loglilihood estimates into a df
#' #'
#' #' Wrapper for make_ll_df
#' #'
#' #' @param list_of_lls multiple outputs of estimate_ts_loglik
#' #'
#' #' @return data frame of loglik, model info
#' #' @export
#' #'
#' bundle_lls <- function(list_of_lls) {
#'
#'   ll_dfs <- lapply(list_of_lls, make_ll_df)
#'
#'   bind_rows(ll_dfs)
#' }
#'
#' singular_ll <- function(many_fits) {
#'
#'   nsims <- many_fits[[1]]$model_info$nit
#'
#'   timestep_means <- lapply(many_fits, FUN = function(fit) return(mean(fit$test_logliks)))
#'
#'   one_ll <- sum(unlist(lapply(many_fits, FUN = function(fits) return(mean(fits$test_logliks)))))
#'
#'   cbind(data.frame(mean_loglik = one_ll), as.data.frame(many_fits[[1]]$model_info))
#' }
#'
#' #' Title
#' #'
#' #' @param ll
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' make_ll_df <- function(ll) {
#'
#'   cbind(data.frame(loglik = ll$loglik_ests), as.data.frame(ll$model_info))
#'
#' }
#'
#' gamma_plot <- function (x, selection = "median", cols = set_gamma_colors(x),
#'                         xname = NULL, together = FALSE, LDATS = FALSE)
#' {
#'   oldpar <- par(no.readonly = TRUE)
#'   on.exit(par(oldpar))
#'   if (LDATS) {
#'     par(fig = c(0, 1, 0, 0.3), new = TRUE)
#'   }
#'   else if (together) {
#'     par(fig = c(0, 1, 0, 0.52), new = TRUE)
#'   }
#'   else {
#'     par(fig = c(0, 1, 0, 1))
#'   }
#'   rhos <- x$rhos
#'   nrhos <- ncol(rhos)
#'   if (!is.null(nrhos)) {
#'     if (selection == "median") {
#'       spec_rhos <- ceiling(apply(rhos, 2, median))
#'     }
#'     else if (selection == "mode") {
#'       spec_rhos <- apply(rhos, 2, modalvalue)
#'     }
#'     else {
#'       stop("selection input not supported")
#'     }
#'   } else {
#'     spec_rhos <- NULL
#'   }
#'   x$control$timename <- NULL
#'   seg_mods <- multinom_TS(x$data, x$formula, spec_rhos, x$timename,
#'                           x$weights, x$control)
#'   nsegs <- length(seg_mods[[1]])
#'   t1 <- min(x$data[, x$timename])
#'   t2 <- max(x$data[, x$timename])
#'   if (is.null(xname)) {
#'     xname <- x$timename
#'   }
#'   par(mar = c(4, 5, 1, 1))
#'   plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "",
#'        xaxt = "n", yaxt = "n", ylim = c(0, 1), xlim = c(t1 -
#'                                                           1, t2 + 1))
#'   yax <- round(seq(0, 1, length.out = 5), 3)
#'   axis(2, at = yax, las = 1)
#'   axis(1)
#'   mtext(side = 2, line = 3.5, cex = 1.25, "Proportion")
#'   mtext(side = 1, line = 2.5, cex = 1.25, xname)
#'   ntopics <- ncol(as.matrix(x$data[[x$control$response]]))
#'   seg1 <- c(0, spec_rhos[-length(rhos)])
#'   seg2 <- c(spec_rhos, t2)
#'   time_obs <- rep(NA, nrow(x$data))
#'   pred_vals <- matrix(NA, nrow(x$data), ntopics)
#'   sp1 <- 1
#'   for (i in 1:nsegs) {
#'     mod_i <- seg_mods[[1]][[i]]
#'     spec_vals <- sp1:(sp1 + nrow(mod_i$fitted.values) -
#'                         1)
#'     pred_vals[spec_vals, ] <- mod_i$fitted.values
#'     time_obs[spec_vals] <- mod_i$timevals
#'     sp1 <- sp1 + nrow(mod_i$fitted.values)
#'   }
#'   for (i in 1:ntopics) {
#'     points(time_obs, pred_vals[, i], type = "l", lwd = 3,
#'            col = cols[i])
#'   }
#'   if (!is.null(spec_rhos)) {
#'     rho_lines(spec_rhos)
#'   }
#' }
#'
#'
#'
#' rho_plot <- function (x)
#' {
#'
#'   rhos <- as.data.frame(x$rhos) %>%
#'     tidyr::pivot_longer(everything(.), names_to = "changepoint", values_to = "estimate", names_prefix = "V") %>%
#'     mutate(changepoint  = as.factor(changepoint))
#'
#'   start <- min(x$data$year)
#'   end <- max(x$data$year)
#'
#'   ggplot(rhos, aes(estimate, group = changepoint, fill  = changepoint)) +
#'     geom_histogram(alpha = .5, position = "identity") +
#'     theme_bw() +
#'     scale_color_viridis_d() +
#'     xlim(c(start, end))
#'
#' }
#'
#'
#' fit_ldats_crossval <- function(dataset, use_folds = F, n_folds = 5, n_timesteps = 2, buffer = 2, k, seed, cpts, nit, fit_to_train = FALSE, fold_seed = 1977) {
#'   if(!is.null(fold_seed)) {
#'     set.seed(fold_seed)
#'   }
#'
#'   all_subsets <- subset_data_all(dataset, use_folds = use_folds, n_folds = n_folds, n_timesteps = n_timesteps, buffer_size = buffer)
#'
#'   all_ldats_fits <- lapply(all_subsets, FUN = ldats_subset_one, k = k, seed = seed, cpts = cpts, nit = nit, fit_to_train = fit_to_train)
#'
#'   return(all_ldats_fits)
#' }
#'
#' eval_ldats_crossval <- function(ldats_fits, nests = 100, use_folds = F) {
#'   if(!use_folds) {
#'     estimates <- estimate_ts_loglik(ldats_fits, nests = nests)
#'
#'     ll_df <- make_ll_df(estimates)
#'
#'     single_ll <- singular_ll(ldats_fits)
#'
#'     ll_df <- dplyr::left_join(ll_df, single_ll)
#'   } else{
#'     ll_df <- make_folds_loglik_df(ldats_fits)
#'   }
#'   return(ll_df)
#'
#' }
#'
#' plot_lda_comp <- function(fitted_lda, specl = F) {
#'
#'   lda_betas <- data.frame(t(fitted_lda[[1]]@beta))
#'
#'   colnames(lda_betas) <- c(1:ncol(lda_betas))
#'
#'   lda_betas$species <- (unlist(fitted_lda[[1]]@terms))
#'
#'   lda_betas <- tidyr::pivot_longer(lda_betas, -species, names_to = "topic", values_to = "beta")
#'
#'   lda_betas$beta <- exp(lda_betas$beta)
#'
#'   betas_plot <- ggplot(lda_betas, aes(species, beta, fill = topic)) +
#'     geom_col(position = "stack") +
#'     theme_void() +
#'     scale_fill_viridis_d(end = .7)
#'
#'   if(specl) {
#'     betas_plot <- betas_plot +
#'       theme(axis.text.x = element_text(angle = 0))
#'   }
#'
#'
#'   return(betas_plot)
#' }
#'
#' plot_lda_year <- function(fitted_lda, covariate_data) {
#'
#'   if(is.list(fitted_lda)) {
#'     fitted_lda <- fitted_lda[[1]]
#'   }
#'
#'   lda_preds <- data.frame(fitted_lda@gamma)
#'
#'   colnames(lda_preds) <- c(1:ncol(lda_preds))
#'
#'   stopifnot(nrow(lda_preds) == length(covariate_data))
#'
#'   lda_preds$year <- covariate_data
#'
#'   lda_preds <- tidyr::pivot_longer(lda_preds, -year, names_to = "topic",values_to = "proportion")
#'
#'
#'   pred_plot <- ggplot(lda_preds, aes(year, proportion, color = topic)) +
#'     geom_line(size = 2) +
#'     theme_bw() +
#'     scale_color_viridis_d(end = .7)
#'
#'   return(pred_plot)
#' }
