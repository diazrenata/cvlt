#' Run LDATs on a dataset using crossvalidation
#'
#' Top-level wrapper to subset a dataset and fit LDATS to the subsets.
#'
#' @param dataset MATSS-style dataset (list with `$abundance` and `$covariates`)
#' @param buffer number of timesteps to withold on either side of the test timestep, default 2
#' @param k integer number of topics
#' @param lda_seed integer seed to use to run LDA
#' @param cpts integer number of changepoints
#' @param nit integer number of iterations for the changepoint model. 100 is fast but will not find the global optimum, 1000 gets closer but takes time.
#' @param cpt_seed integer which seed to use for the cpt model. If NULL (default) randomly drawn.
#' @param return_full logical whether to return the model objects or just the logliks. Returning the objects hogs memory. Defaults FALSE.
#' @param return_fits logical. If TRUE returns list with fits. If FALSE (default) returns a dataframe of model info and the loglikelihood estimated as the sum (over all test steps) of the mean loglikelihood (over all iterations) for each step.
#' @param summarize_ll logical. If TRUE, summary dataframe will have only one row of model info and the loglikelihood estimated as the sum (over all test steps) of the mean loglikelihood (over all iterations) for each step. If FALSE, summary dataframe will return all time steps. FALSE useful for diagnostics.
#'
#' @return If return_fits = TRUE, list of lists. Each element is the result of running `ldats_subset_one` on one subset of the original dataset. If return_fits = FALSE, returns a dataframe of model info and the mean loglikelihood (over all iterations) for each step.
#' @export
#' @importFrom dplyr bind_rows group_by summarize ungroup
fit_ldats_crossval <- function(dataset, buffer = 2, k, lda_seed, cpts, nit, cpt_seed = NULL, return_full = FALSE, return_fits = FALSE, summarize_ll = TRUE) {

  all_subsets <- subset_data_all(dataset, buffer_size = buffer)

  all_ldats_fits <- lapply(all_subsets, FUN = ldats_subset_one, k = k, lda_seed = lda_seed, cpts = cpts, nit = nit, cpt_seed = cpt_seed, return_full = return_full)

  if(return_fits) {

    return(all_ldats_fits)

  } else {

    all_summary <- summarize_ldats_fit(all_ldats_fits, summarize_ll = summarize_ll)

  }

  return(all_summary)

}
