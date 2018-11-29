context("mlr_measures")

test_that("mlr_measures", {
  ids = mlr_measures$keys()

  for (key in ids) {
    m = mlr_measures$get(key)
    expect_measure(m)
  }
})
