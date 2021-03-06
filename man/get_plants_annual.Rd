% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_short_portal.R
\name{get_plants_annual}
\alias{get_plants_annual}
\title{Get Portal plants}
\usage{
get_plants_annual(census_season = "winter")
}
\arguments{
\item{census_season}{"winter" or "summer"}
}
\value{
MATSS-style dataset: list with elements $abundance and $covariates.
}
\description{
Get abundance data for winter or summer annuals on control plots at Portal, formatted for LDATS.
}
