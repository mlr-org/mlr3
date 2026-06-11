test_that("LearnerRegr predict_newdata_fast response works", {
  learner = lrn("regr.debug")
  task = tsk("mtcars")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "se", "quantiles"))
  expect_numeric(pred$response)
})

test_that("LearnerRegr predict_newdata_fast se works", {
  learner = lrn("regr.debug", predict_type = "se")
  task = tsk("mtcars")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "se", "quantiles"))
  expect_numeric(pred$se)
})

test_that("LearnerRegr predict_newdata_fast quantiles works", {
  learner = lrn("regr.debug", predict_type = "quantiles")
  learner$quantiles = c(0.1, 0.5, 0.9)
  learner$quantile_response = 0.5
  task = tsk("mtcars")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "se", "quantiles"))
  expect_matrix(pred$quantiles, nrows = nrow(newdata), ncols = length(learner$quantiles))
})

test_that("LearnerRegr predict_newdata_fast works with missing values", {
  learner = lrn("regr.debug", predict_missing = 0.5)
  learner$encapsulate("evaluate", fallback = lrn("regr.featureless"))
  task = tsk("mtcars")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "se", "quantiles"))
  expect_numeric(pred$response, any.missing = FALSE)

  learner = lrn("regr.debug", predict_missing = 0.5, predict_type = "se")
  learner$encapsulate("evaluate", fallback = lrn("regr.featureless", predict_type = "se"))
  task = tsk("mtcars")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "se", "quantiles"))
  expect_numeric(pred$se, any.missing = FALSE)

  learner = lrn("regr.debug", predict_missing = 0.5, predict_type = "quantiles")
  learner$quantiles = c(0.1, 0.5, 0.9)
  learner$quantile_response = 0.5
  fallback = lrn("regr.featureless", predict_type = "quantiles")
  fallback$quantiles = c(0.1, 0.5, 0.9)
  fallback$quantile_response = 0.5
  learner$encapsulate("evaluate", fallback = fallback)
  task = tsk("mtcars")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "se", "quantiles"))
  expect_matrix(pred$quantiles, nrows = nrow(newdata), ncols = length(learner$quantiles), any.missing = FALSE)
})

test_that("LearnerRegr predict_newdata_fast works with failed train", {
  learner = lrn("regr.debug", predict_missing = 0.5, error_train = 1)
  learner$encapsulate("evaluate", fallback = lrn("regr.featureless"))
  task = tsk("mtcars")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "se", "quantiles"))
  expect_numeric(pred$response, any.missing = FALSE)

  learner = lrn("regr.debug", predict_missing = 0.5, predict_type = "se", error_train = 1)
  learner$encapsulate("evaluate", fallback = lrn("regr.featureless", predict_type = "se"))
  task = tsk("mtcars")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "se", "quantiles"))
  expect_numeric(pred$se, any.missing = FALSE)

  learner = lrn("regr.debug", predict_missing = 0.5, predict_type = "quantiles", error_train = 1)
  learner$quantiles = c(0.1, 0.5, 0.9)
  learner$quantile_response = 0.5
  fallback = lrn("regr.featureless", predict_type = "quantiles")
  fallback$quantiles = c(0.1, 0.5, 0.9)
  fallback$quantile_response = 0.5
  learner$encapsulate("evaluate", fallback = fallback)
  task = tsk("mtcars")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)
})

test_that("LearnerRegr predict_newdata_fast restores fallback state after parallel resample", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")

  task = tsk("mtcars")
  newdata = task$data()

  # parallel execution sends the learner across a process boundary, so the trained fallback survives
  # only in the learner state, not on the fallback object itself
  resample_parallel = function(...) {
    learner = lrn("regr.debug", ...)
    learner$encapsulate("evaluate", fallback = lrn("regr.featureless"))
    rr = with_future(future.callr::callr, resample(task, learner, rsmp("holdout"), store_models = TRUE))
    l = rr$learners[[1]]
    expect_true(is.null(l$fallback$state$model))
    l
  }

  # train fully failed -> wholesale fallback
  pred = resample_parallel(error_train = 1)$predict_newdata_fast(newdata, task)
  expect_numeric(pred$response, len = nrow(newdata), any.missing = FALSE)

  # partial missing predictions -> fallback imputation
  pred = resample_parallel(predict_missing = 0.5)$predict_newdata_fast(newdata, task)
  expect_numeric(pred$response, len = nrow(newdata), any.missing = FALSE)
})
