library(cvlt)
rats <- get_rodents_annual()
rats_ss <- subset_data_all(rats)

a_subset <- rats_ss[[16]]
#
# buffer= 2
# k = 2
# cpts = 1
# nit =100
# seed =4
#
# #dataset <- rats

# rats_fits <- fit_ldats_crossval(rats, k = 2, lda_seed = 4, cpts = 1, nit = 50, summarize_ll = T)
rats_fits <- fit_multiple_ldats_crossval(rats, ks = c(2), lda_seeds = c(2), cpts = c(0), nit = 50, summarize_ll = T)

load("svethis.RData")


selmod <- select_cvlt(rats_fits)

bestmod <- run_best_model(rats, dplyr::mutate(selmod, cpts = 0))

# bestmodts <- modal_ts(bestmod)
#
# modal_ts_preds =bestmodts

s <- summarize_model(bestmod)



winter_CC <- soar::get_plants_annual_ldats()

winter_CC_bestmod <- data.frame(k = 2, lda_seed = 2, cpts = 1)

winter_CC_mod <- run_best_model(winter_CC, winter_CC_bestmod)

winter_CC_summary <- summarize_model(winter_CC_mod)



winter_EE <- soar::get_plants_annual_ldats(plot_type = "EE")

winter_EE_bestmod <- data.frame(k = 2, lda_seed = 2, cpts = 1)

winter_EE_mod <- run_best_model(winter_EE, winter_EE_bestmod)

winter_EE_summary <- summarize_model(winter_EE_mod)




summer_CC <- soar::get_plants_annual_ldats(census_season = "summer")

summer_CC_bestmod <- data.frame(k = 2, lda_seed = 6, cpts = 4)

summer_CC_mod <- run_best_model(summer_CC, summer_CC_bestmod)

summer_CC_summary <- summarize_model(summer_CC_mod)

summer_EE <- soar::get_plants_annual_ldats(census_season = "summer", plot_type = "EE")

summer_EE_bestmod <- data.frame(k = 2, lda_seed = 4, cpts = 0)

summer_EE_mod <- run_best_model(summer_EE, summer_EE_bestmod)

summer_EE_summary <- summarize_model(summer_EE_mod)
