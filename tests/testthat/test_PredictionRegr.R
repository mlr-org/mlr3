context("PredictionRegr")

test_that("Construction", {
  task = mlr_tasks$get("boston_housing")
  p = PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth())
  expect_prediction(p)
  expect_prediction_regr(p)
})

test_that("Internally constructed Prediction", {
  task = mlr_tasks$get("boston_housing")
  lrn = mlr_learners$get("regr.featureless")
  lrn$predict_type = "se"
  e = Experiment$new(task, lrn)$train()
  e$predict()
  p = e$prediction
  expect_prediction(p)
  expect_prediction_regr(p)
})


test_that("c", {
  task = mlr_tasks$get("boston_housing")
  lrn = mlr_learners$get("regr.featureless")
  lrn$predict_type = "se"
  rr = resample(task, lrn, "cv3")

  pred = do.call(c, map(rr$experiments(), "prediction"))
  expect_prediction(pred)
  expect_prediction_regr(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrow = task$nrow, ncol = 4L, any.missing = FALSE)
})

test_that("c drops se (#250)", {
  task = mlr_tasks$get("boston_housing")
  lrn = mlr_learners$get("regr.featureless")
  rr = resample(task, lrn, "cv3")

  pred = do.call(c, map(rr$experiments(), "prediction"))
  expect_null(pred$data$se)
  expect_false("se" %in% pred$predict_types)
  expect_true(allMissing(pred$se))
  expect_false("se" %in% names(as.data.table(pred)))
})

test_that("merge", {
  task = mlr_tasks$get("mtcars")
  lrn = mlr_learners$get("regr.featureless", predict_type = "se")

  e = Experiment$new(task, lrn)$train()$predict()
  x = e$prediction$clone()
  y = e$prediction$clone()

  x$data$response[1:5] = NA
  x$data$se[10:12] = NA
  z = merge(x, y)
  expect_false(anyMissing(z$response))
  expect_false(anyMissing(z$se))
})
