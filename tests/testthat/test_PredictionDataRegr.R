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

test_that("truth is checked", {
  pdata = new_prediction_data(list(row_ids = 1:4, truth = c(1, 2), response = c(1, 1, 1, 1)), "regr")
  expect_error(check_prediction_data(pdata), "truth")

  pdata = new_prediction_data(list(row_ids = 1:2, truth = c("a", "b"), response = c(1, 1)), "regr")
  expect_error(check_prediction_data(pdata), "truth")

  pdata = new_prediction_data(list(row_ids = 1:3, response = c(1, 1, 1)), "regr")
  expect_equal(check_prediction_data(pdata)$truth, rep(NA_real_, 3))
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

test_that("construction of empty PredictionDataRegr with quantiles", {
  task = tsk("mtcars")

  learner = lrn("regr.featureless", predict_type = "quantiles")
  learner$quantiles = c(0.1, 0.5, 0.9)
  learner$quantile_response = 0.5
  learner$train(task)
  pred = learner$predict(task, row_ids = integer())
  expect_prediction(pred)
  expect_set_equal(pred$predict_types, c("response", "quantiles"))
  expect_matrix(pred$quantiles, nrows = 0L, ncols = 3L)
  expect_equal(attr(pred$quantiles, "probs"), c(0.1, 0.5, 0.9))
  expect_numeric(pred$response, len = 0L)
  expect_data_table(as.data.table(pred), nrows = 0L, ncols = 6L)

  combined = c(pred, learner$predict(task, row_ids = 1:3))
  expect_prediction(combined)
  expect_equal(combined$row_ids, 1:3)
  expect_matrix(combined$quantiles, nrows = 3L, ncols = 3L)

  resampling = rsmp("custom")
  resampling$instantiate(task, train_sets = list(1:20, 1:20), test_sets = list(21:32, integer()))
  rr = resample(task, learner, resampling)
  expect_equal(rr$prediction()$row_ids, 21:32)
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

test_that("quantile attributes are checked", {
  make_pdata = function(probs, response) {
    quantiles = matrix(as.numeric(1:6), nrow = 2)
    setattr(quantiles, "probs", probs)
    setattr(quantiles, "response", response)
    new_prediction_data(list(row_ids = 1:2, truth = c(1, 2), quantiles = quantiles), "regr")
  }

  expect_error(check_prediction_data(make_pdata(c(0.1, 0.5, 0.9), 0.3)), "not one of the predicted quantiles")
  expect_error(check_prediction_data(make_pdata(c(0.1, 0.5), 0.5)), "3 columns")
  expect_error(check_prediction_data(make_pdata(c(0.1, 0.5, 0.5), 0.5)), "unique")

  pdata = check_prediction_data(make_pdata(c(0.1, 0.5, 0.9), 0.5))
  expect_equal(colnames(pdata$quantiles), c("q0.1", "q0.5", "q0.9"))
  expect_equal(attr(pdata$quantiles, "response"), "q0.5")
})

test_that("check_prediction_data.PredictionDataRegr works on checked quantiles", {
  task = tsk("mtcars")
  learner = lrn("regr.featureless", predict_type = "quantiles", quantiles = c(0.1, 0.5, 0.9), quantile_response = 0.5)
  pdata = learner$train(task)$predict(task)$data

  pred = as_prediction(pdata, check = TRUE)
  expect_prediction_regr(pred)
  expect_equal(attr(pred$data$quantiles, "response"), "q0.5")
  expect_number(pred$score(msr("regr.pinball", alpha = 0.1)))
})

test_that("c.PredictionDataRegr preserves probs attribute on quantiles", {
  n = 10
  probs = c(0.1, 0.5, 0.9)
  y = runif(n)
  task = as_task_regr(data.table(y = y), target = "y")

  quantiles = quantile(y, probs = probs)
  quantiles = matrix(rep(quantiles, n), nrow = n, byrow = TRUE)
  setattr(quantiles, "probs", probs)
  setattr(quantiles, "response", 0.5)

  pdata = as_prediction_data(list(quantiles = quantiles), task)
  combined = c(pdata, pdata)

  expect_equal(attr(combined$quantiles, "probs"), probs)
  expect_equal(attr(combined$quantiles, "response"), "q0.5")

  combined = c(pdata, pdata, keep_duplicates = FALSE)

  expect_equal(attr(combined$quantiles, "probs"), probs)
  expect_equal(attr(combined$quantiles, "response"), "q0.5")
})

test_that("c.PredictionDataRegr rejects different quantiles", {
  task = as_task_regr(data.table(y = runif(2)), target = "y")
  make_pdata = function(probs, response) {
    quantiles = matrix(seq_len(2 * length(probs)), nrow = 2)
    setattr(quantiles, "probs", probs)
    setattr(quantiles, "response", response)
    as_prediction_data(list(quantiles = quantiles), task)
  }
  pdata = make_pdata(c(0.1, 0.5, 0.9), 0.5)
  expect_error(c(pdata, make_pdata(c(0.2, 0.5, 0.8), 0.5)), "Different quantiles")
  expect_error(c(pdata, make_pdata(c(0.1, 0.5, 0.9), 0.9)), "Different quantiles")
})

test_that("filter_prediction_data.PredictionDataRegr preserves probs attribute on quantiles", {
  n = 10
  probs = c(0.1, 0.5, 0.9)
  y = runif(n)
  task = as_task_regr(data.table(y = y), target = "y")

  quantiles = quantile(y, probs = probs)
  quantiles = matrix(rep(quantiles, n), nrow = n, byrow = TRUE)
  setattr(quantiles, "probs", probs)
  setattr(quantiles, "response", 0.5)

  pdata = as_prediction_data(list(quantiles = quantiles), task)
  filtered = filter_prediction_data(pdata, 1:3)

  expect_equal(attr(filtered$quantiles, "probs"), probs)
  expect_equal(attr(filtered$quantiles, "response"), "q0.5")

  pred = as_prediction(pdata)$filter(1:3)
  expect_number(pred$score(msr("regr.pinball", alpha = 0.1)))
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

test_that("combine with extra data", {
  pdata = new_prediction_data(
    list(
      row_ids = 1:2,
      truth = runif(2),
      response = runif(2),
      extra = list(extra_col = c("a", "b"), extra_col2 = c(1, 2))
    ),
    "regr"
  )

  pdata2 = new_prediction_data(
    list(
      row_ids = 1:2,
      truth = runif(2),
      response = runif(2),
      extra = list(extra_col = c("c", "d"), extra_col2 = c(3, 4))
    ),
    "regr"
  )

  expect_equal(c(pdata, pdata2)$extra, list(extra_col = c("a", "b", "c", "d"), extra_col2 = c(1, 2, 3, 4)))
  expect_equal(c(pdata, pdata2, keep_duplicates = FALSE)$extra, list(extra_col = c("c", "d"), extra_col2 = c(3, 4)))

  pdata2 = new_prediction_data(
    list(
      row_ids = 1:2,
      truth = runif(2),
      response = runif(2),
      extra = list(extra_col = c("c", "d"), extra_col3 = c(3, 4))
    ),
    "regr"
  )

  expect_equal(
    c(pdata, pdata2)$extra,
    list(extra_col = c("a", "b", "c", "d"), extra_col2 = c(1, 2, NA, NA), extra_col3 = c(NA, NA, 3, 4))
  )

  pdata2 = new_prediction_data(
    list(
      row_ids = 1:2,
      truth = runif(2),
      response = runif(2),
      prob = matrix(c(0.5, 0.5, 0.5, 1), 2)
    ),
    "regr"
  )

  expect_error(c(pdata, pdata2), "Some predictions have extra data, others do not")
})

test_that("filtering with extra data", {
  pdata = new_prediction_data(
    list(
      row_ids = 1:2,
      truth = runif(2),
      response = runif(2),
      extra = list(extra_col = c("a", "b"), extra_col2 = c(1, 2))
    ),
    "regr"
  )

  expect_equal(filter_prediction_data(pdata, row_ids = 1)$extra, list(extra_col = "a", extra_col2 = 1))
  expect_equal(filter_prediction_data(pdata, row_ids = 2)$extra, list(extra_col = "b", extra_col2 = 2))
})
