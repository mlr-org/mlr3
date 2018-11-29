context("mlr_control")

test_that("mlr_control", {
  ctrl = mlr_control()
  expect_list(ctrl, names = "unique")

  ctrl = mlr_control(debug = TRUE)
  expect_list(ctrl, names = "unique")
  expect_true(ctrl$debug)

  ctrl = mlr_control(debug = FALSE)
  expect_list(ctrl, names = "unique")
  expect_false(ctrl$debug)
})
