% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lda_set_user_seeds.R
\name{LDA_set_user_seeds}
\alias{LDA_set_user_seeds}
\title{Run a set of Latent Dirichlet Allocation models with user-specified seed}
\usage{
LDA_set_user_seeds(document_term_table, topics = 2, seed = 1, control = list())
}
\arguments{
\item{document_term_table}{Table of observation count data (rows:
documents, columns: terms. May be a class \code{matrix} or
\code{data.frame} but must be conformable to a matrix of integers,
as verified by \code{\link{check_document_term_table}}.}

\item{topics}{Vector of the number of topics to evaluate for each model.
Must be conformable to \code{integer} values.}

\item{seed}{Seed to use for each
value of \code{topics}.}

\item{control}{A \code{list} of parameters to control the running and
selecting of LDA models. Values not input assume default values set
by \code{\link{LDA_set_control}}. Values for running the LDAs replace
defaults in (\code{LDAcontol}, see \code{\link[topicmodels]{LDA}} (but if
\code{seed} is given, it will be overwritten; use \code{iseed} instead).}
}
\value{
\code{LDA_set}: \code{list} (class: \code{LDA_set}) of LDA models
(class: \code{LDA_VEM}).
\code{check_LDA_set_inputs}: an error message is thrown if any input is
improper, otherwise \code{NULL}.
}
\description{
This is a modification to the function LDATS::LDA_set to allow the user to choose the seed for the LDA. RMD originally added this function in a branch of weecology/LDATS, but that means that to use it you have to install that branch version of LDATS. It is necessary for this package, so porting it over as part of \code{cvlt}. This means \code{cvlt} can depend on the CRAN version of LDATS.

From LDATS documentation:

For a given dataset consisting of counts of words across
multiple documents in a corpus, conduct multiple Latent Dirichlet
Allocation (LDA) models (using the Variational Expectation
Maximization (VEM) algorithm; Blei \emph{et al.} 2003) to account for \link{1}
uncertainty in the number of latent topics and \link{2} the impact of initial
values in the estimation procedure. \cr \cr
\code{LDA_set} is a list wrapper of \code{\link[topicmodels]{LDA}}
in the \code{topicmodels} package (Grun and Hornik 2011). \cr \cr
\code{check_LDA_set_inputs} checks that all of the inputs
are proper for \code{LDA_set} (that the table of observations is
conformable to a matrix of integers, the number of topics is an integer,
the number of seeds is an integer and the controls list is proper).
}
\references{
Blei, D. M., A. Y. Ng, and M. I. Jordan. 2003. Latent Dirichlet
Allocation. \emph{Journal of Machine Learning Research}
\strong{3}:993-1022.
\href{http://jmlr.csail.mit.edu/papers/v3/blei03a.html}{link}.

Grun B. and K. Hornik. 2011. topicmodels: An R Package for Fitting Topic
Models. \emph{Journal of Statistical Software} \strong{40}:13.
\href{https://www.jstatsoft.org/article/view/v040i13}{link}.
}
