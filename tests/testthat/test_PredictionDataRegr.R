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
