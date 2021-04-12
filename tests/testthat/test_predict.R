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

test_that("missing predictions are handled gracefully", {
  task = tsk("sonar")
  learner = lrn("classif.debug", predict_missing = 1, predict_missing_type = "na", predict_type = "prob")

  learner$train(task)
  p = learner$predict(task)
  expect_factor(p$response, levels = task$class_names)
  expect_true(all(is.na(p$response)))

  expect_true(all(is.na(p$prob)))


  learner = lrn("classif.debug", predict_missing = 0.5, predict_missing_type = "omit", predict_type = "prob")
  learner$train(task)
  expect_error(learner$predict(task), "observations")
})
