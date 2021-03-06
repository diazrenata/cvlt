#### COPIED FROM LDATS branch lda-user-seed.

#' @title Run a set of Latent Dirichlet Allocation models with user-specified seed
#'
#' @description This is a modification to the function LDATS::LDA_set to allow the user to choose the seed for the LDA. RMD originally added this function in a branch of weecology/LDATS, but that means that to use it you have to install that branch version of LDATS. It is necessary for this package, so porting it over as part of `cvlt`. This means `cvlt` can depend on the CRAN version of LDATS.
#'
#'  From LDATS documentation:
#'
#'  For a given dataset consisting of counts of words across
#'   multiple documents in a corpus, conduct multiple Latent Dirichlet
#'   Allocation (LDA) models (using the Variational Expectation
#'   Maximization (VEM) algorithm; Blei \emph{et al.} 2003) to account for [1]
#'   uncertainty in the number of latent topics and [2] the impact of initial
#'   values in the estimation procedure. \cr \cr
#'   \code{LDA_set} is a list wrapper of \code{\link[topicmodels]{LDA}}
#'   in the \code{topicmodels} package (Grun and Hornik 2011). \cr \cr
#'   \code{check_LDA_set_inputs} checks that all of the inputs
#'   are proper for \code{LDA_set} (that the table of observations is
#'   conformable to a matrix of integers, the number of topics is an integer,
#'   the number of seeds is an integer and the controls list is proper).
#'
#' @param document_term_table Table of observation count data (rows:
#'   documents, columns: terms. May be a class \code{matrix} or
#'   \code{data.frame} but must be conformable to a matrix of integers,
#'   as verified by \code{\link{check_document_term_table}}.
#'
#' @param topics Vector of the number of topics to evaluate for each model.
#'   Must be conformable to \code{integer} values.
#'
#' @param seed Seed to use for each
#'   value of \code{topics}.
#'
#' @param control A \code{list} of parameters to control the running and
#'   selecting of LDA models. Values not input assume default values set
#'   by \code{\link{LDA_set_control}}. Values for running the LDAs replace
#'   defaults in (\code{LDAcontol}, see \code{\link[topicmodels]{LDA}} (but if
#'    \code{seed} is given, it will be overwritten; use \code{iseed} instead).
#'
#' @return
#'   \code{LDA_set}: \code{list} (class: \code{LDA_set}) of LDA models
#'   (class: \code{LDA_VEM}).
#'   \code{check_LDA_set_inputs}: an error message is thrown if any input is
#'   improper, otherwise \code{NULL}.
#'
#' @references
#'   Blei, D. M., A. Y. Ng, and M. I. Jordan. 2003. Latent Dirichlet
#'   Allocation. \emph{Journal of Machine Learning Research}
#'   \strong{3}:993-1022.
#'   \href{http://jmlr.csail.mit.edu/papers/v3/blei03a.html}{link}.
#'
#'   Grun B. and K. Hornik. 2011. topicmodels: An R Package for Fitting Topic
#'   Models. \emph{Journal of Statistical Software} \strong{40}:13.
#'   \href{https://www.jstatsoft.org/article/view/v040i13}{link}.
#'
#' @export
#' @importFrom LDATS check_LDA_set_inputs LDA_msg prep_LDA_control package_LDA_set LDA_set_control
#' @importFrom topicmodels LDA
LDA_set_user_seeds <- function(document_term_table, topics = 2, seed = 1,
                               control = list()){
  nseeds = length(seed)
  LDATS::check_LDA_set_inputs(document_term_table, topics, nseeds = nseeds, control)
  control <- do.call("LDA_set_control_cv", control)
  mod_topics <- rep(topics, each = length(seq(2, length(seed) * 2, 2)))
  iseed <- seed
  mod_seeds <- rep(seq(iseed, iseed + (nseeds - 1)* 2, 2), length(topics))
  nmods <- length(mod_topics)
  mods <- vector("list", length = nmods)
  for (i in 1:nmods){
    LDATS::LDA_msg(mod_topics[i], mod_seeds[i], control)
    control_i <- LDATS::prep_LDA_control(seed = mod_seeds[i], control = control)
    mods[[i]] <- topicmodels::LDA(document_term_table, k = mod_topics[i],
                     control = control_i)
  }
  LDATS::package_LDA_set(mods, mod_topics, mod_seeds)
}


#' @title Create control list for set of LDA models
#'
#' @description This function provides a simple creation and definition of
#'   the list used to control the set of LDA models. It is set up to be easy
#'   to work with the existing control capacity of
#'   \code{\link[topicmodels]{LDA}}. **Copied straight from weecology/LDATS**; need it as part of this namespace to work with `do.call` in `LDA_set_user_seeds`
#'
#' @param quiet \code{logical} indicator of whether the model should run
#'   quietly.
#'
#' @param measurer,selector Function names for use in evaluation of the LDA
#'   models. \code{measurer} is used to create a value for each model
#'   and \code{selector} operates on the values to choose the model(s) to
#'   pass on.
#'
#' @param iseed \code{integer} initial seed for the model set.
#'
#' @param ... Additional arguments to be passed to
#'   \code{\link[topicmodels]{LDA}} as a \code{control} input.
#'
#' @return \code{list} for controlling the LDA model fit.
#'
#' @export
#'
LDA_set_control_cv <- function(quiet = FALSE, measurer = AIC, selector = min,
                            iseed = 2, ...){
  list(quiet = quiet, measurer = measurer, selector = selector,
       iseed = iseed, ...)
}
