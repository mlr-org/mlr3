context("mlr_control")

test_that("mlr_control", {
  ctrl = mlr_control()
  expect_list(ctrl, names = "unique")

  ctrl = mlr_control(store_model = TRUE)
  expect_list(ctrl, names = "unique")
  expect_true(ctrl$store_model)

  ctrl = mlr_control(store_model = FALSE)
  expect_list(ctrl, names = "unique")
  expect_false(ctrl$store_model)
}
)
