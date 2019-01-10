context("Prediction")

test_that("Construction", {
  p = Prediction$new()
  expect_prediction(p)
})
