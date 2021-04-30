library(testthat)
library(cvlt)
test_dir("testthat", reporter = c("check", "progress"))

test_check("cvlt")
