#' Subset data - all subsets
#'
#' Wrapper for `subset_data_one` to create a list of all data subsets using one timestep of test data per subset and a buffer on either side.
#'
#' @param full_dataset MATSS-style dataset. A list with elements `$abundance`, `$covariates`
#' @param buffer_size number of timesteps to withhold on either side of the test timestep. Defaults 2.
#'
#' @return a list of ntimesteps lists, if ntimesteps is the number of timesteps in `full_dataset`. Each list in the list is the output of `subset_data_one` using one of the timesteps in the full dataset as the test timestep.
#' @export
#'

subset_data_all <- function(full_dataset, buffer_size = 2) {

    subsetted_data = lapply(1:nrow(full_dataset$abundance), FUN = subset_data_one, full_dataset = full_dataset, buffer_size = buffer_size)

  return(subsetted_data)
}

#' Subset data - one subset
#'
#' Creates a *single* training/test subset for a dataset.
#' Witholds a single timestep for testing, and a buffer of timesteps around that timestep.
#'
#' @param full_dataset MATSS style dataset. A list with elements `$abundance`, `$covariates`
#' @param test_timestep Which timestep (row) to withold and use for test
#' @param buffer_size How many rows to withold on either side (not used for test)
#'
#' @return list with elements train (list of $abundance, $covariates), test (list of $abundance, $covariates), full (unaltered full_dataset), test_timestep (which tstep is the test one), buffer_size (buffer size)
#' @export
#'
subset_data_one <- function(full_dataset, test_timestep, buffer_size) {

  timesteps_to_withold <- c((test_timestep - buffer_size):(test_timestep + buffer_size))
  timesteps_to_withold <- timesteps_to_withold[
    which(timesteps_to_withold %in% 1:nrow(full_dataset$abundance))]

  timesteps_to_keep <- 1:nrow(full_dataset$abundance)
  timesteps_to_keep <- timesteps_to_keep[ which(!(timesteps_to_keep %in% timesteps_to_withold  ))]

  test_data <- list(
    abundance = full_dataset$abundance[ test_timestep, ],
    covariates = full_dataset$covariates[ test_timestep, ]
  )

  train_data <- list(
    abundance = full_dataset$abundance[ timesteps_to_keep, ],
    covariates = full_dataset$covariates[ timesteps_to_keep, ])

  subsetted_dataset <- list(
    train = train_data,
    test = test_data,
    full = full_dataset,
    test_timestep = test_timestep,
    buffer_size = buffer_size
  )

  return(subsetted_dataset)

}

#' #' #### The following is scrap. It's for subsetting with folds (randomly selecting multiple years + a buffer) rather than systematically subsetting by removing each year.
#' #' Subset data - one subset
#' #'
#' #' Creates a *single* training/test subset for a dataset.
#' #' Witholds a single timestep for testing, and a buffer of timesteps around that timestep.
#' #'
#' #' @param full_dataset MATSS style dataset. A list with elements `$abundance`, `$covariates`
#' #' @param test_timestep Which timestep (row) to withold and use for test
#' #' @param buffer_size How many rows to withold on either side (not used for test)
#' #'
#' #' @return list with elements train (list of $abundance, $covariates), test (list of $abundance, $covariates), full (unaltered full_dataset), test_timestep (which tstep is the test one), buffer_size (buffer size)
#' #' @export
#' #'
#' subset_data_one_folds <- function(full_dataset, n_timesteps, buffer_size) {
#'   test_timesteps <- vector()
#'
#'   available_timesteps <- 1:nrow(full_dataset$abundance)
#'   last_timestep <- nrow(full_dataset$abundance)
#'
#'   ntries <- 0
#'
#'   while(all(length(test_timesteps) < n_timesteps ,ntries < 100)){
#'     candidate <- sample(available_timesteps, 1)
#'
#'     candidate_buffer <- c((candidate - buffer_size):(candidate + buffer_size))
#'
#'     candidate_buffer <- candidate_buffer[ which(candidate_buffer %in% 1:last_timestep)]
#'
#'     if(all(candidate_buffer %in% available_timesteps)) {
#'       available_timesteps <- available_timesteps[ which(!(available_timesteps %in% candidate_buffer))]
#'       test_timesteps <- c(test_timesteps, candidate)
#'     }
#'
#'     ntries <- ntries + 1
#'   }
#'
#'   stopifnot(length(test_timesteps) == n_timesteps)
#'
#'   timesteps_to_withold <- vector()
#'   for(i in 1:length(test_timesteps)) {
#'     timesteps_to_withold <- c(timesteps_to_withold, c((test_timesteps[i] - buffer_size):(test_timesteps[i] + buffer_size)))
#'   }
#'
#'   timesteps_to_withold <- timesteps_to_withold[
#'     which(timesteps_to_withold %in% 1:nrow(full_dataset$abundance))]
#'
#'   timesteps_to_keep <- 1:nrow(full_dataset$abundance)
#'   timesteps_to_keep <- timesteps_to_keep[ which(!(timesteps_to_keep %in% timesteps_to_withold  ))]
#'
#'   test_data <- list(
#'     abundance = full_dataset$abundance[ test_timesteps, ],
#'     covariates = full_dataset$covariates[ test_timesteps, ]
#'   )
#'
#'   train_data <- list(
#'     abundance = full_dataset$abundance[ timesteps_to_keep, ],
#'     covariates = full_dataset$covariates[ timesteps_to_keep, ])
#'
#'   subsetted_dataset <- list(
#'     train = train_data,
#'     test = test_data,
#'     full = full_dataset,
#'     test_timestep = test_timesteps,
#'     buffer_size = buffer_size
#'   )
#'
#'   return(subsetted_dataset)
#'
#' }
