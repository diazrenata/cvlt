test_that("trivial", {

  expect_true(TRUE)


}
)
test_that("portal annual works", {

  portal_annual <- get_rodents_annual()

  expect_true(length(portal_annual) == 2)

  expect_true(names(portal_annual)[1] == "abundance")
  expect_true(names(portal_annual)[2] == "covariates")

  expect_true(is.data.frame(portal_annual$abundance))
  expect_true(is.data.frame(portal_annual$covariates))

  expect_equal(nrow(portal_annual$abundance), nrow(portal_annual$covariates))

  expect_true("year" %in% colnames(portal_annual$covariates))

  expect_equal(ncol(portal_annual$abundance), 21)
  expect_equal(ncol(portal_annual$covariates), 2)

  expect_false(anyNA(portal_annual$abundance))

  expect_true(all(portal_annual$covariates$year == c(1979:2019)))
}
)


