test_that("expecations do not use globals", {
  skip_if_not_installed("codetools")

  ee = new.env()
  path = system.file("testthat", "helper_expectations.R", package = "mlr3")
  sys.source(path, envir = ee)

  testthat::expect_silent(codetools::checkUsageEnv(ee))
})
