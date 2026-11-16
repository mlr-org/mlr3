test_that("mlr_measures_regr.pinball", {
  task = tsk("california_housing")
  lrn = lrn("regr.featureless")

  expect_error(msr("regr.pinball", alpha = 2), "alpha: Element 1 is not <= 1")

  m = msr("regr.pinball")
  expect_equal(m$properties, "weights")
  preds_na = lrn$train(task)$predict(task)
  expect_warning(preds_na$score(m), "missing predict type 'quantiles'")
  score_na = suppressWarnings(unname(preds_na$score(m)))
  expect_equal(score_na, NaN)

  lrn$predict_type = "quantiles"
  lrn$quantiles = c(0.25, 0.5, 0.75)
  lrn$quantile_response = 0.5

  preds = lrn$train(task)$predict(task)
  expect_number(preds$score(m))

  # alpha must be in predicted quantiles
  expect_error(preds$score(msr("regr.pinball", alpha = 0.1)), "needs predictions for the quantile 0.1")
})

test_that("regr.pinball matches alpha with a tolerance", {
  task = tsk("mtcars")
  learner = lrn("regr.featureless", predict_type = "quantiles", quantiles = seq(0.1, 0.9, by = 0.1))
  learner$quantile_response = 0.5
  prediction = learner$train(task)$predict(task)
  expect_false(attr(prediction$data$quantiles, "probs")[3L] == 0.3)

  expect_equal(
    unname(prediction$score(msr("regr.pinball", alpha = 0.3))),
    mlr3measures::pinball(prediction$truth, prediction$data$quantiles[, 3L], alpha = 0.3)
  )
})
