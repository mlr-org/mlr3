# this test / files was missing, only classif.debug was unit-tested
# I added at least a few basic tests when i added methods "importance" and "selected_features"

test_that("Simple training/predict", {
  task = tsk("mtcars")
  learner = lrn("regr.debug")
  expect_learner(learner, task)

  prediction = learner$train(task)$predict(task)
  expect_class(learner$model, "regr.debug_model")
  expect_numeric(learner$model$response, len = 1L, any.missing = FALSE)
  expect_numeric(prediction$response, any.missing = FALSE)
})


test_that("importance and selected features", {
  l = lrn("regr.debug")
  task = tsk("mtcars")
  l$train(task)
  expect_equal(l$selected_features(), character(0))
  expect_equal(l$importance(), set_names(rep(0, task$n_features), task$feature_names))
})

test_that("weights are respected", {
  # Task with weights_learner role defined in helper_misc.R
  task = cars_weights_learner
  learner = lrn("regr.debug")

  # Check learner property
  expect_true("weights" %in% learner$properties)

  # Train with response prediction
  learner$train(task)

  # Calculate expected weighted mean and sd manually
  dt = task$data()
  expected_mean = weighted.mean(dt$dist, task$weights_learner$weight)

  # Check model components
  expect_equal(learner$model$response, expected_mean)
  w = task$weights_learner$weight
  expected_sd = sqrt(sum(w * (dt$dist - expected_mean)^2) / (sum(w) - sum(w^2) / sum(w)))
  expect_equal(learner$model$se, expected_sd)

  learner$predict_type = "se"
  p = learner$train(task)$predict(task)
  expect_equal(p$se, rep(expected_sd, task$nrow))
  learner$predict_type = "response"

  # Train with quantile prediction
  learner$predict_type = "quantiles"
  learner$quantiles = c(0.25, 0.5, 0.75)
  learner$quantile_response = 0.5
  learner$train(task)

  # Calculate expected weighted quantiles manually
  expected_quantiles = quantile_weighted(dt$dist, probs = c(0.25, 0.5, 0.75), weights = task$weights_learner$weight)

  # Check model components for quantiles
  expect_equal(learner$model$quantiles, unname(expected_quantiles))
  expect_equal(learner$model$quantile_probs, c(0.25, 0.5, 0.75))
})

test_that("quantiles work", {
  task = tsk("mtcars")
  learner = lrn("regr.debug", predict_type = "quantiles")
  learner$quantiles = c(0.25, 0.5, 0.75)
  learner$quantile_response = 0.5
  learner$train(task)
  expect_equal(learner$quantiles, c(0.25, 0.5, 0.75))
  expect_equal(learner$quantile_response, 0.5)

  prediction = learner$predict(tsk("mtcars"))
  expect_matrix(prediction$quantiles, nrows = nrow(task$data()), ncols = length(learner$quantiles), any.missing = FALSE)
  expect_numeric(prediction$response, len = nrow(task$data()), any.missing = FALSE)

  learner = lrn("regr.debug", predict_type = "quantiles")
  expect_error(learner$train(task), "Quantiles must be set")

  learner$quantiles = c(0.25, 0.5, 0.75)
  expect_error(learner$train(task), "Quantile response must be set")
})

test_that("NA predictions", {
  task = tsk("mtcars")
  learner = lrn("regr.debug", predict_missing = 0.5)
  p = learner$train(task)$predict(task)
  expect_equal(count_missing(p$response), 16L)

  learner = lrn("regr.debug", predict_missing = 0.5, predict_type = "se")
  p = learner$train(task)$predict(task)
  expect_equal(count_missing(p$response), 16L)
  expect_equal(is.na(p$response), is.na(p$se))
})
  
test_that("condition switches", {
  task = tsk("mtcars")
  expect_message(lrn("regr.debug", message_train = 1)$train(task), "Message from regr.debug->train()", fixed = TRUE)
  expect_warning(lrn("regr.debug", warning_train = 1)$train(task), "Warning from regr.debug->train()", fixed = TRUE)
  expect_error(
    lrn("regr.debug", error_train = 1)$train(task),
    "regr.debug->train()",
    fixed = TRUE,
    class = "Mlr3ErrorLearnerTrain"
  )

  learner = lrn("regr.debug")$train(task)
  learner$param_set$set_values(message_predict = 1)
  expect_message(learner$predict(task), "Message from regr.debug->predict()", fixed = TRUE)
  learner$param_set$set_values(message_predict = 0, warning_predict = 1)
  expect_warning(learner$predict(task), "Warning from regr.debug->predict()", fixed = TRUE)
  learner$param_set$set_values(warning_predict = 0, error_predict = 1)
  expect_error(learner$predict(task), "regr.debug->predict()", fixed = TRUE, class = "Mlr3ErrorLearnerPredict")
})

test_that("save_tasks", {
  task = tsk("mtcars")
  learner = lrn("regr.debug", save_tasks = TRUE)
  learner$train(task, row_ids = 1:20)
  learner$predict(task, row_ids = 21:32)
  expect_equal(learner$model$task_train$row_ids, 1:20)
  expect_equal(learner$model$task_predict$row_ids, 21:32)
})

test_that("omitted predictions", {
  task = tsk("mtcars")
  learner = lrn("regr.debug", predict_missing = 0.25, predict_missing_type = "omit")
  learner$train(task)
  expect_error(learner$predict(task), "for 8 observations is missing")

  learner$param_set$set_values(predict_missing = 0)
  expect_prediction(learner$predict(task))
})
