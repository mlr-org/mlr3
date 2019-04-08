context("mlr_measures")

test_that("mlr_measures", {
  keys = mlr_measures$keys()

  for (key in keys) {
    m = mlr_measures$get(key)
    expect_measure(m)
  }
})
