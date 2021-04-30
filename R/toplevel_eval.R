
eval_ldats_crossval <- function(ldats_fits, nests = 100, use_folds = F) {
  if(!use_folds) {
    estimates <- estimate_ts_loglik(ldats_fits, nests = nests)

    ll_df <- make_ll_df(estimates)

    single_ll <- singular_ll(ldats_fits)

    ll_df <- dplyr::left_join(ll_df, single_ll)
  } else{
    ll_df <- make_folds_loglik_df(ldats_fits)
  }
  return(ll_df)

}
