% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_run_helpers.R
\name{get_one_test_loglik}
\alias{get_one_test_loglik}
\title{Get test row logliklihood (for one draw)}
\usage{
get_one_test_loglik(subsetted_dataset_item, abund_probabilities_one)
}
\arguments{
\item{subsetted_dataset_item}{list with - at least - elements \code{test} (of abundance matrix and covaraites for test timestep) and \code{test_timestep} (the row number, of the full dataset, of the test timestep).}

\item{abund_probabilities_one}{ONE matrix of abundance probabilities}
}
\value{
numeric, loglikelihood of the observed abundances from the test timestep given abund_probabilities
}
\description{
Get loglikelihood of observed abundances for test timestep given predicted abundance probabilities, for a single draw from the posterior.
}
