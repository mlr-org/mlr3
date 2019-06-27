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

  # duplicates are detected?
  p1 = rr$experiment(1)$prediction
  p2 = rr$experiment(1)$prediction
  p3 = c(p1, p2, keep_duplicates = FALSE)
  expect_equal(sort(p1$data$row_ids), sort(p2$data$row_ids))
  expect_equal(sort(p1$data$row_ids), sort(p3$data$row_ids))
  expect_numeric(p3$response, len = length(p1$response), any.missing = FALSE)
  expect_numeric(p3$se, len = length(p1$se), any.missing = FALSE)
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
