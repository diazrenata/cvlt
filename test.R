library(cvlt)
rats <- get_rodents_annual()
rats_ss <- subset_data_all(rats)

subsetted_dataset_item <- rats_ss[[16]]

k = 2
cpts = 1
nit = 100
seed = 7

fitted_lda <- LDA_set_user_seeds(
  document_term_table = subsetted_dataset_item$full$abundance,
  topics = k,
  seed = seed)[[1]]

subsetted_lda <- subset_lda(fitted_lda, subsetted_dataset_item)
fitted_ts <- LDATS::TS_on_LDA(subsetted_lda,
                              document_covariate_table = as.data.frame(subsetted_dataset_item$train$covariates),
                              timename = "year",
                              formulas = ~1,
                              nchangepoints = cpts,
                              control = LDATS::TS_control(nit = nit))[[1]]
fitted_lda <- subsetted_lda

abund_probabilities <- get_abund_probabilities(
  subsetted_dataset_item,
  subsetted_lda,
  fitted_ts
)


# Calculate loglikelihood of test timestep for each draw from the posterior
test_logliks  <- get_test_loglik(
  subsetted_dataset_item,
  abund_probabilities
)
