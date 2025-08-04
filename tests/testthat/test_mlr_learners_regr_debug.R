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
