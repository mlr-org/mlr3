test_that("mlr_measures_regr.pinball", {
  task = tsk("california_housing")
  lrn = lrn("regr.featureless")

  expect_error(msr("regr.pinball", alpha = 2), "alpha: Element 1 is not <= 1")

  m = msr("regr.pinball")
  expect_equal(m$properties, character(0))
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
  expect_error(preds$score(msr("regr.pinball", alpha = 0.1)),
               "Assertion on 'alpha' failed: Must be element of set")
})
