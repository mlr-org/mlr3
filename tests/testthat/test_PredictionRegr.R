context("PredictionRegr")

test_that("Construction", {
  task = tsk("boston_housing")
  p = PredictionRegr$new(row_ids = task$row_ids, truth = task$truth(), response = task$truth())
  expect_prediction(p)
  expect_prediction_regr(p)
})

test_that("Internally constructed Prediction", {
  task = tsk("boston_housing")
  lrn = lrn("regr.featureless")
  lrn$predict_type = "se"
  p = lrn$train(task)$predict(task)
  expect_prediction(p)
  expect_prediction_regr(p)
})


test_that("c", {
  task = tsk("boston_housing")
  lrn = lrn("regr.featureless")
  lrn$predict_type = "se"
  rr = resample(task, lrn, rsmp("cv", folds = 3))

  pred = do.call(c, map(rr$data$prediction, "test"))
  expect_prediction(pred)
  expect_prediction_regr(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrows = task$nrow, ncols = 4L, any.missing = FALSE)

  # duplicates are detected?
  p1 = rr$data$prediction[[1L]]$test
  p2 = rr$data$prediction[[1L]]$test
  p3 = c(p1, p2, keep_duplicates = FALSE)
  expect_equal(sort(p1$data$row_ids), sort(p2$data$row_ids))
  expect_equal(sort(p1$data$row_ids), sort(p3$data$row_ids))
  expect_numeric(p3$response, len = length(p1$response), any.missing = FALSE)
  expect_numeric(p3$se, len = length(p1$se), any.missing = FALSE)
})

test_that("c drops se (#250)", {
  task = tsk("boston_housing")
  lrn = lrn("regr.featureless")
  rr = resample(task, lrn, rsmp("cv", folds = 3))

  pred = do.call(c, map(rr$data$prediction, "test"))
  expect_null(pred$data$se)
  expect_false("se" %in% pred$predict_types)
  expect_true(allMissing(pred$se))
  expect_false("se" %in% names(as.data.table(pred)))
})

test_that("distr", {
  skip_if_not_installed("distr6")

  distr = distr6::VectorDistribution$new(
    distribution = "Binomial",
    params = list(
      list(prob = 0.1, size = 2),
      list(prob = 0.6, size = 4),
      list(prob = 0.2, size = 6)
    )
  )

  task = tsk("mtcars")
  p = PredictionRegr$new(task, distr = distr)
  expect_prediction(p)
  expect_prediction(c(p, p))
  expect_output(print(p))
  expect_integer(p$missing, len = 0)
})
