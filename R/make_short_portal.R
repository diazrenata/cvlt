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
