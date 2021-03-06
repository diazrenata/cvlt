% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_run_helpers.R
\name{multinom_theta}
\alias{multinom_theta}
\title{Get theta matrix for one draw from posterior}
\usage{
multinom_theta(subsetted_dataset_item, ts_model, sim = 1)
}
\arguments{
\item{subsetted_dataset_item}{list with (at least) element \verb{$full}: unaltered dataset.}

\item{ts_model}{TS fit to either the full dataset or a subset}

\item{sim}{integer, which draw from the posterior to extract the thetas for}
}
\value{
matrix of predicted topic proportions (columns) for each timestep in the full dataset (rows)
}
\description{
The theta matrix is the predicted topic proportions.
}
