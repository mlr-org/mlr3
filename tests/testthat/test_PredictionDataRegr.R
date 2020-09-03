context("PredictionDataRegr")


test_that("PredictionDataRegr", {
  task = tsk("mtcars")
  learner = lrn("regr.featureless", predict_type = "se")
  p = learner$train(task)$predict(task)
  pdata = p$data

  expect_is(pdata, "PredictionDataRegr")
  expect_integer(pdata$row_id, any.missing = FALSE)
  expect_numeric(pdata$truth, any.missing = FALSE)
  expect_numeric(pdata$response, any.missing = FALSE)
  expect_numeric(pdata$se, any.missing = FALSE)

  expect_is(c(pdata, pdata), "PredictionDataRegr")
  expect_prediction(as_prediction(pdata))
  expect_equal(as.data.table(p), as.data.table(as_prediction(pdata)))
})
