context("PredictionClassif")

test_that("Construction", {
  p = PredictionClassif$new()
  expect_prediction(p)
})

test_that("partial results", {
  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  e = Experiment$new(task, lrn)$train()$predict()
  p = e$prediction
  expect_prediction(p)
  expect_prediction_classif(p, task = task)

  p = PredictionClassif$new()
  expect_prediction(p)
  expect_prediction_classif(p)
  p$row_ids = task$row_ids
  expect_error(as.data.table(p))

  p$truth = task$truth()
  p$response = rep(factor("setosa", levels = task$all_classes), task$nrow)
  expect_prediction(p)
  expect_prediction_classif(p)
})
