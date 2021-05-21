#' Summarize a model fit
#'
#' Extract summary info
#'
#' @param model_fit list of `$dataset`, `$lda_mod`, `$ts_mod`
#'
#' @return dataframe with info about the lda and ts models
#' @export
#'
#' @importFrom dplyr mutate group_by_all ungroup left_join
summarize_model <- function(model_fit) {

  if(model_fit$config$cpts == 0) {
    cpt_summary <- data.frame(
      Mean = NA,
      Median = NA,
      Mode = NA,
      `Lower_95%` = NA,
      `Upper_95%` = NA,
      SD = NA,
      MCMCerr = NA,
      AC10 = NA,
      ESS = 0,
      cpt = NA,
      nyears = length(unique(model_fit$dataset$covariates$year)),
      width = NA,
      width_ratio = NA,
      modal_estimate = NA
    )
  } else {

  cpt_summary <- model_fit$ts_mod$rho_summary %>%
    dplyr::mutate(cpt = row.names(model_fit$ts_mod$rho_summary)) %>%
    dplyr::group_by_all() %>%
    dplyr::mutate(cpt = unlist(strsplit(cpt, split = "_"))[2]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nyears = length(unique(model_fit$dataset$covariates$year))) %>%
    dplyr::mutate(width =`Upper_95%` - `Lower_95%`) %>%
    dplyr::mutate(width_ratio = width / nyears) %>%
    dplyr::mutate(modal_estimate = find_modal_cpts(model_fit))
}
  ts_preds <- modal_ts(model_fit)

  cpt_distance <- changepoint_dissimilarity(ts_preds)

  cpt_summary <- cpt_summary %>%
    dplyr::left_join(cpt_distance)

  model_summary <- model_fit$config %>%
    dplyr::bind_cols(cpt_summary)

  return(model_summary)

}

#' Find modal combined location of changepoints
#'
#' Find most common JOINT combination of changepoint locations from a TS fit.
#'
#' @param model_fit list with `$ts_mod`
#'
#' @return vector of changepoint locations (years)
#' @export
#'
#' @importFrom dplyr group_by_all mutate n ungroup row_number filter select distinct
find_modal_cpts <- function(model_fit) {

  common_rhos <- model_fit$ts_mod$rhos %>%
    as.data.frame() %>%
    dplyr::group_by_all() %>%
    dplyr::mutate(tally = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(draw = dplyr::row_number())

  if(nrow(common_rhos) == 0) {
    cpts = NULL
  } else {

    modal_rhos <- common_rhos %>%
      dplyr::filter(tally == max(common_rhos$tally)) %>%
      dplyr::select(-tally, -draw) %>%
      dplyr::distinct()

    cpts = as.vector(unlist(modal_rhos[1,]))
  }

  return(cpts)
}

#' Predictions from TS model using modal changepoint estimates
#'
#' Get predicted topic proportions, species proportions, or species abundances from the modal changepoint estimates from a TS model.
#'
#'
#' @param model_fit list with elements `$dataset`, `$lda_mod`, `$ts_mod`
#' @param type "topic_proportions", "species_proportions", or "species_abundances"
#'
#' @return dataframe of predicted values by year and segment
#' @export
#'
#' @importFrom dplyr mutate group_by_all n ungroup row_number filter select distinct bind_rows
#' @importFrom LDATS multinom_TS
modal_ts <- function(model_fit, type = "species_proportions") {

  cpts <- find_modal_cpts(model_fit)


  modal_ts <- LDATS::multinom_TS(data = model_fit$ts_mod$data, model_fit$ts_mod$formula, changepoints = cpts, timename = "year")

  modal_ts_fits <- list()

  for(i in 1:length(modal_ts[[1]])) {
    modal_ts_fits[[i]] <- as.data.frame(modal_ts[[1]][[i]]$fitted.values) %>%
      dplyr::mutate(timestep = row.names(.))
  }

  modal_ts_fits <- dplyr::bind_rows(modal_ts_fits, .id = "seg")

  modal_ts_fits <- modal_ts_fits %>%
    dplyr::mutate(year = model_fit$ts_mod$data$year)

  if(type == "topic_proportions") {
    return(modal_ts_fits)
  }

  modal_thetas <- modal_ts_fits %>%
    dplyr::select(-seg, -timestep, -year) %>%
    as.matrix()

  modal_betas <- exp(model_fit$lda_mod@beta)

  modal_abundance_preds <- modal_thetas %*% modal_betas

  fitted_data <- modal_abundance_preds %>%
    as.data.frame()
  colnames(fitted_data) <- colnames(model_fit$dataset$abundance)

  fitted_data <- fitted_data %>%
    dplyr::mutate(year = model_fit$ts_mod$data$year) %>%
    dplyr::left_join(dplyr::distinct(dplyr::select(modal_ts_fits, year, seg)))

  if(type == "species_proportions") {
    return(fitted_data)
  }


  actual_abundances <- rowSums(model_fit$dataset$abundance)

  fitted_abundances <- dplyr::select(fitted_data, -c(seg, year))

  for(i in 1:nrow(fitted_abundances)) {
    fitted_abundances[i, ] = fitted_abundances[i, ] * actual_abundances[i]
  }

  fitted_abundances <- fitted_abundances %>%
    dplyr::mutate(year = model_fit$ts_mod$data$year) %>%
    dplyr::left_join(dplyr::distinct(dplyr::select(modal_ts_fits, year, seg)))

  if(type == "species_abundances") {
    return(fitted_abundances)
  }

}

#' Change in community composition across changepoints
#'
#' Bray-Curtis dissimilarity comparing the community composition pre and post each changepoint.
#'
#' @param modal_ts_preds dataframe of species abundances (or proportions) with columns `year` and `seg`. Result of `modal_ts()`
#'
#' @return dataframe of `cpt`, `seg_before`, `seg_after`, and `dissimilarity`
#' @export
#'
#' @importFrom dplyr select distinct
#' @importFrom vegan vegdist
changepoint_dissimilarity <- function(modal_ts_preds) {

  ncpts <- length(unique(modal_ts_preds$seg)) - 1

  segment_fitted_values <- modal_ts_preds  %>%
    dplyr::select(-year) %>%
    dplyr::distinct()

  segment_matrix <- segment_fitted_values %>%
    dplyr::select(-seg) %>%
    as.matrix()

  segment_bc <- as.matrix(vegan::vegdist(segment_matrix, method = "bray"))

  if(ncpts > 0) {

    changepoint_distance <- data.frame(cpt = c(1:(ncpts))) %>%
    dplyr::mutate(seg_before = cpt,
                  seg_after = cpt + 1,
                  dissimilarity = NA)

    for(i in 1:nrow(changepoint_distance)) {
      changepoint_distance$dissimilarity[i] <- segment_bc[changepoint_distance$seg_before[i], changepoint_distance$seg_after[i]]
    }

    changepoint_distance <- changepoint_distance %>%
      dplyr::mutate(cpt = as.character(cpt))
  } else {

    changepoint_distance <- data.frame(cpt = NA,
                                       seg_before = NA,
                                       seg_after = NA,
                                       dissimilarity = NA)
  }

  return(changepoint_distance)

}
