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
