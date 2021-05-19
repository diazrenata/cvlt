#' Run LDATs on a dataset using crossvalidation
#'
#' Top-level wrapper to subset a dataset and fit LDATS to the subsets.
#'
#' @param dataset MATSS-style dataset (list with `$abundance` and `$covariates`)
#' @param buffer number of timesteps to withold on either side of the test timestep, default 2
#' @param k integer number of topics
#' @param seed integer seed to use to run LDA
#' @param cpts integer number of changepoints
#' @param nit integer number of iterations for the changepoint model. 100 is fast but will not find the global optimum, 1000 gets closer but takes time.
#'
#' @return list of lists. Each element is the result of running `ldats_subset_one` on one subset of the original dataset.
#' @export
#'
fit_ldats_crossval <- function(dataset, buffer = 2, k, seed, cpts, nit) {

  all_subsets <- subset_data_all(dataset, buffer_size = buffer)

  all_ldats_fits <- lapply(all_subsets, FUN = ldats_subset_one, k = k, seed = seed, cpts = cpts, nit = nit)

  return(all_ldats_fits)
}
