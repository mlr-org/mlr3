context("predict")

test_that("predict method works", {
  task = tsk("sonar")
  lrn = lrn("classif.featureless")$train(task)

  newdata = task$data(1:3)
  expect_factor(predict(lrn, newdata = newdata), len = 3)
  expect_factor(predict(lrn, newdata = newdata, predict_type = "response"), len = 3)
  expect_error(predict(lrn, newdata = newdata, predict_type = "prob"), "'prob'")

  lrn = lrn("classif.featureless", predict_type = "prob")$train(task)
  expect_factor(predict(lrn, newdata = newdata), len = 3)
  expect_factor(predict(lrn, newdata = newdata, predict_type = "response"), len = 3)
  expect_matrix(predict(lrn, newdata = newdata, predict_type = "prob"), nrows = 3, ncols = 2)

  expect_true(uniqueN(predict(lrn, newdata, method = "mode")) == 1L)
})
