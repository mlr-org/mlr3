test_that("PredictionDataRegr", {
  task = tsk("mtcars")
  learner = lrn("regr.featureless", predict_type = "se")
  p = learner$train(task)$predict(task)
  pdata = p$data

  expect_s3_class(pdata, "PredictionDataRegr")
  expect_integer(pdata$row_ids, any.missing = FALSE)
  expect_numeric(pdata$truth, any.missing = FALSE)
  expect_numeric(pdata$response, any.missing = FALSE)
  expect_numeric(pdata$se, any.missing = FALSE)

  expect_s3_class(c(pdata, pdata), "PredictionDataRegr")
  expect_prediction(as_prediction(pdata))
  expect_equal(as.data.table(p), as.data.table(as_prediction(pdata)))

  pdata = filter_prediction_data(pdata, row_ids = 1:3)
  expect_set_equal(pdata$row_ids, 1:3)
  expect_numeric(pdata$truth, len = 3)
  expect_numeric(pdata$response, len = 3)
})

test_that("construction of empty PredictionDataRegr", {
  task = tsk("mtcars")

  learner = lrn("regr.featureless")
  learner$train(task)
  pred = learner$predict(task, row_ids = integer())
  expect_prediction(pred)
  expect_set_equal(pred$predict_types, "response")
  expect_integer(pred$row_ids, len = 0L)
  expect_numeric(pred$truth, len = 0L)
  expect_null(pred$data$se)
  expect_null(pred$data$distr)
  expect_data_table(as.data.table(pred), nrows = 0L, ncols = 3L)

  learner = lrn("regr.featureless", predict_type = "se")
  learner$train(task)
  pred = learner$predict(task, row_ids = integer())
  expect_prediction(pred)
  expect_set_equal(pred$predict_types, c("response", "se"))
  expect_integer(pred$row_ids, len = 0L)
  expect_numeric(pred$truth, len = 0L)
  expect_numeric(pred$se, len = 0L)
  expect_data_table(as.data.table(pred), nrows = 0L, ncols = 4L)
})

test_that("PredictionDataRegr with quantiles", {
  n = 100
  probs = c(0.1, 0.5, 0.9)
  y = runif(n)
  task = as_task_regr(data.table(y = y), target = "y")

  quantiles = quantile(y, probs = probs)
  quantiles = matrix(rep(quantiles, n), nrow = n, byrow = TRUE)
  setattr(quantiles, "probs", probs)
  setattr(quantiles, "response", 0.5)

  data = list(quantiles = quantiles)
  pdata = as_prediction_data(data, task)

  pred = as_prediction(pdata)
  expect_prediction_regr(pred)
})

test_that("PredictionDataRegr with quantiles and response", {
  n = 100
  probs = c(0.1, 0.9)
  y = runif(n)
  task = as_task_regr(data.table(y = y), target = "y")

  quantiles = quantile(y, probs = probs)
  quantiles = matrix(rep(quantiles, n), nrow = n, byrow = TRUE)
  setattr(quantiles, "probs", probs)

  data = list(quantiles = quantiles, response = rep(0.5, n))
  pdata = as_prediction_data(data, task)

  pred = as_prediction(pdata)
  expect_prediction_regr(pred)
})
