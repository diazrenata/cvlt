#' Select simplest good model
#'
#' Select simplest fit within one SE of best fit
#'
#' First minimizes the number of changepoints, then the number of topics. Then selects the LDA seed with the best loglikelihood.
#'
#'
#' @param cvlt_fits result of running fit_ldats_multiple
#'
#' @return dataframe with specifications for simplest model with loglikelihood within 1 SE of the loglikelihood of the best model
#' @export
#'
#' @importFrom dplyr filter
select_cvlt <- function(cvlt_fits) {

best_loglik <- max(cvlt_fits$mean_loglik)

best_model <- dplyr::filter(cvlt_fits, mean_loglik == best_loglik)

best_options <- dplyr::filter(cvlt_fits,
                                    mean_loglik > (best_model$mean_loglik - best_model$se_loglik))

fewest_cpts <- min(best_options$cpts)

best_options <- best_options %>%
  dplyr::filter(cpts == fewest_cpts)

fewest_k <- min(best_options$k)

best_options <- best_options %>%
  dplyr::filter(k == fewest_k)

highest_ll <- max(best_options$mean_loglik)

best_options <- best_options %>%
  dplyr::filter(mean_loglik == highest_ll)

if(best_options$k[1] == 0) {
  best_options <- best_options[1,]
}

return(best_options)

}


#' Run best-fitting model
#'
#' Run the LDATS model matching the best configuration for a dataset
#'
#' @param dataset dataset
#' @param selected_config result of select_cvlt; or dataframe with 1 row of `k`, `lda_seed`, `cpts`
#' @param nit default 1000, nit
#' @param cpt_seed optionally pass a seed to the cpt model, default NULL
#'
#' @return list with dataset, config, LDA, and TS models
#' @export
#'
#' @importFrom LDATS TS_on_LDA TS_control
run_best_model <- function(dataset, selected_config, nit = 1000, cpt_seed = NULL) {

  if(selected_config$k[1] > 0) {

  this_lda <- LDA_set_user_seeds(dataset$abundance, topics = selected_config$k[1], seed = selected_config$lda_seed[1])[[1]]
  } else if(selected_config$k[1] == 0) {
    this_lda <- fit_means_lda(dataset, lda_seed = selected_config$lda_seed[1])
  }

  if(is.null(cpt_seed)) {
    cpt_seed = sample.int(1000000000, 1)
  }

  this_ts <- LDATS::TS_on_LDA(this_lda,
                              document_covariate_table = as.data.frame(dataset$covariates),
                              timename = "year",
                              formulas = ~1,
                              nchangepoints = selected_config$cpts[1],
                              control = LDATS::TS_control(nit = nit, seed = cpt_seed))[[1]]

  return(list(
    dataset = dataset,
    config = selected_config,
    lda_mod = this_lda,
    ts_mod = this_ts
  ))

}
