library(cvlt)
rats <- get_rodents_annual()
rats_ss <- subset_data_all(rats)

a_subset <- rats_ss[[16]]

buffer= 2
k = 2
cpts = 1
nit =100
seed =4

#dataset <- rats

rats_fits <- fit_ldats_crossval(rats, k = 2, lda_seed = 4, cpts = 1, nit = 50, summarize_ll = T)
