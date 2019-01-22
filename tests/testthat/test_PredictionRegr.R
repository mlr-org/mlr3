context("PredictionClassif")

test_that("Construction", {
  p = PredictionClassif$new()
  expect_prediction(p)
})

test_that("partial results", {
  task = mlr_tasks$get("bh")
  lrn = mlr_learners$get("regr.featureless")
  lrn$predict_type = "se"
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  expect_prediction(p)
  expect_prediction_regr(p)

  p = PredictionRegr$new()
  expect_prediction(p)
  expect_prediction_regr(p)
  p$row_ids = task$row_ids
  expect_error(as.data.table(p))

  p$truth = task$truth()
  p$response = rep(100, task$nrow)
  expect_prediction(p)
  expect_prediction_regr(p)
})
