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
#' make_ll_df <- function(ll) {
#'
#'   cbind(data.frame(loglik = ll$loglik_ests), as.data.frame(ll$model_info))
#'
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
#' estimate_fold_loglik <- function(one_fold, summary = "mean") {
#'   if(summary == "mean") {
#'     return(mean(one_fold$test_logliks))
#'   }
#' }
