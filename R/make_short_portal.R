#' Get Portal rodent data annually
#'
#' Gets Portal control rodents, summarized as annual sums. For methods development.
#'
#' @return list of abundance and covariates in `MATSS` format
#' @export
#'
#' @importFrom portalr abundance
#' @importFrom dplyr filter mutate select group_by summarize_all ungroup
get_rodents_annual <- function() {

abund <- portalr::abundance(time = "year", level = "Treatment")


abund_annual <- abund %>%
  dplyr::filter(treatment == "control") %>%
  dplyr::mutate(year = (substr(censusdate, 0, 4))) %>%
  dplyr::select(-newmoonnumber, -period, -censusdate, -treatment) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize_all(sum) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::filter(year <= 2018)

abundance <- dplyr::select(abund_annual, -year)
covariates <- dplyr::select(abund_annual, year) %>%
  dplyr::mutate(col2 = "dummycol")

abund_dat <- list(abundance = abundance,
                 covariates = covariates)

return(abund_dat)

}

#' Get Portal plants
#'
#' @param census_season "winter" or summmer
#'
#' @return ready for ldats
#' @export
#'
#' @importFrom portalr plant_abundance
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr pivot_wider
get_plants_annual <- function(census_season = "winter") {


  if(census_season == "winter") {
  quadrats <- portalr::plant_abundance(level = "Treatment", type = "Winter Annuals", plots = "longterm")
  } else {
    quadrats <- portalr::plant_abundance(level = "Treatment", type = "Summer Annuals", plots = "longterm")

}

  quadrats_wide <- quadrats %>%
    dplyr::filter(treatment == "control",
                  season ==census_season,
                  quads == 64) %>%
    tidyr::pivot_wider(id_cols = year, names_from = species, values_from = abundance, values_fill = 0)

  abundance <- dplyr::select(quadrats_wide, -year)
  covariates <- dplyr::select(quadrats_wide, year) %>%
    dplyr::mutate(col2 = "dummycol")

  abund_dat <- list(abundance = abundance,
                    covariates = covariates)

  return(abund_dat)
  }
