test_that("mlr_measures_regr.rqr", {
  tsk = tsk("california_housing")
  lrn = lrn("regr.featureless")

  expect_error(msr("regr.rqr", alpha = 2), "alpha: Element 1 is not <= 1")

  # default
  m = msr("regr.rqr")
  expect_null(m$properties)
  expect_equal(m$param_set$values$alpha, 0.5)

  # missing predict type
  preds_na = lrn$train(tsk)$predict(tsk)
  expect_warning(preds_na$score(m), "missing predict type 'quantiles'")
  score_na = suppressWarnings(unname(preds_na$score(m)))
  expect_equal(score_na, NaN)

  # proper quantile prediction
  lrn$predict_type = "quantiles"
  lrn$quantiles = c(0.25, 0.5, 0.75)
  lrn$quantile_response = 0.5
  preds = lrn$train(tsk)$predict(tsk)

  expect_number(preds$score(m))
  expect_true(preds$score(m) == 0)

  # pred_set_mean
  m2 = msr("regr.rqr", pred_set_mean = FALSE)
  expect_equal(m2$properties, c("requires_task", "requires_train_set"))
  expect_number(preds$score(m2, task = tsk, train_set = tsk$nrow))

  m_25 = msr("regr.rqr", alpha = 0.25)
  expect_number(preds$score(m_25))

  # alpha must be in predicted quantiles
  expect_error(preds$score(msr("regr.pinball", alpha = 0.1)),
               "Must be element of set")
})
