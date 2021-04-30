# fit_ldats_crossval <- function(dataset, use_folds = F, n_folds = 5, n_timesteps = 2, buffer = 2, k, seed, cpts, nit, fit_to_train = FALSE, fold_seed = 1977) {
#   if(!is.null(fold_seed)) {
#     set.seed(fold_seed)
#   }
#
#   all_subsets <- subset_data_all(dataset, use_folds = use_folds, n_folds = n_folds, n_timesteps = n_timesteps, buffer_size = buffer)
#
#   all_ldats_fits <- lapply(all_subsets, FUN = ldats_subset_one, k = k, seed = seed, cpts = cpts, nit = nit, fit_to_train = fit_to_train)
#
#   return(all_ldats_fits)
# }
#
#
#
#
