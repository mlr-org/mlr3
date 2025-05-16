test_that("autotest", {
  learner = lrn("regr.featureless")
  expect_learner(learner)
  result = run_autotest(learner, exclude = "sanity")
  expect_true(result, info = result$error)
})

test_that("regr.featureless works on featureless task", {
  task = tsk("mtcars")$select(character())
  learner = lrn("regr.featureless")
  rr = resample(task, learner, rsmp("holdout"))
  expect_resample_result(rr)
  expect_number(rr$aggregate())
})

test_that("regr.featureless quantile prediction works", {
  task = tsk("mtcars")

  learner = lrn("regr.featureless",
    predict_type = "quantiles",
    quantiles = c(0.1, 0.5, 0.9),
    quantile_response = 0.5)

  learner$train(task)
  expect_numeric(learner$model$quantiles, len = 3L)

  pred = learner$predict(task)
  expect_prediction(pred)
  expect_subset("quantiles", pred$predict_types)
  expect_matrix(pred$quantiles, ncols = 3L, nrows = task$nrow, any.missing = FALSE)
  expect_names(colnames(pred$quantiles), identical.to = c("q0.1", "q0.5", "q0.9"))
  expect_equal(pred$response, pred$quantiles[, 2L])

  learner = lrn("regr.featureless",
    predict_type = "quantiles")

  expect_error(learner$train(task), "Quantiles")
})

test_that("weights are respected", {
  # Task with weights_learner role defined in helper_misc.R
  task = cars_weights_learner
  learner = lrn("regr.featureless")

  # Check learner property
  expect_true("weights" %in% learner$properties)

  # --- Test robust = FALSE ---
  learner$param_set$values$robust = FALSE
  learner$predict_type = "response" # Reset to default for training
  learner$train(task)

  # Calculate expected weighted mean and sd manually
  dt = task$data()
  w = task$weights_learner$weight
  expected_mean = weighted.mean(dt$dist, w)

  # Check model components
  expect_equal(learner$model$location, expected_mean)

  # Test quantiles with robust = FALSE
  learner$predict_type = "quantiles"
  learner$quantiles = c(0.2, 0.8)
  learner$quantile_response = 0.5 # Needs to be set, even if not used here
  learner$train(task)
  expected_quantiles_nonrobust = quantile_weighted(dt$dist, probs = c(0.2, 0.5, 0.8), weights = w)
  expect_equal(learner$model$quantiles, expected_quantiles_nonrobust)

  # --- Test robust = TRUE ---
  learner$param_set$values$robust = TRUE
  learner$predict_type = "response" # Reset for training
  learner$train(task)

  # Calculate expected weighted median and mad manually
  expected_median = quantile_weighted(dt$dist, probs = 0.5, weights = w, continuous = FALSE)
  expected_mad = quantile_weighted(abs(dt$dist - expected_median), probs = 0.5, weights = w, continuous = FALSE) * 1.4826

  # Check model components
  expect_equal(learner$model$location, expected_median)
  expect_equal(learner$model$dispersion, expected_mad)

  # Test quantiles with robust = TRUE (should be the same as non-robust)
  learner$predict_type = "quantiles"
  learner$quantiles = c(0.3, 0.7)
  learner$quantile_response = 0.5 # Needs to be set
  learner$train(task)
  expected_quantiles_robust = quantile_weighted(dt$dist, probs = c(0.3, 0.5, 0.7), weights = w)
  expect_equal(learner$model$quantiles, expected_quantiles_robust)
})


test_that("weighted quantile helper function", {
  wq = quantile_weighted(c(0, 1), weights = c(1, 2), probs = c(-.Machine$double.eps, 0, 1 / 6, 1 / 3, 0.5, 1, 1 + .Machine$double.eps))

  expect_equal(unname(wq), c(0, 0, 1 / 3, 2 / 3, 3 / 4, 1, 1))

  wqdisc = quantile_weighted(c(0, 1), weights = c(1, 2), probs = c(-.Machine$double.eps, 0, 1 / 6, 1 / 3, 0.5, 1, 1 + .Machine$double.eps), continuous = FALSE)
  expect_equal(unname(wqdisc), c(0, 0, 0, 2 / 3, 1, 1, 1))

  expect_equal(unname(quantile_weighted(c(3, 4, 1, 2), weights = c(1, 1, 1, 1), probs = 0.5, continuous = FALSE)), 2.5)
  expect_equal(unname(quantile_weighted(c(3, 4, 1, 2), weights = c(1, 1, 1, 2), probs = 0.5, continuous = FALSE)), 2)
  expect_equal(unname(quantile_weighted(c(3, 4, 1, 2), weights = c(2, 1, 1, 2), probs = 0.5, continuous = FALSE)), 2.5)
  expect_equal(unname(quantile_weighted(c(3, 4, 1, 2), weights = c(1, 2, 1, 2), probs = 0.5, continuous = FALSE)), 2 + 1 / 3)
})
