context("mlr_resampling_custom")

test_that("custom has duplicated ids", {
  r = mlr_resamplings$get("custom")
  expect_identical(r$duplicated_ids, TRUE)
})
