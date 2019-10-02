context("fallback")

test_that("train fails gracefully", {
  task = tsk("iris")
  learner = lrn("classif.debug", error_train = 1)
  expect_error(learner$train(task), "classif.debug")
})

test_that("predict fails gracefully", {
  task = tsk("iris")
  learner = lrn("classif.debug", error_predict = 1)
  learner$train(task)
  expect_error(learner$predict(task), "classif.debug")
})

test_that("fail during train", {
  task = tsk("iris")
  learner = lrn("classif.debug", error_train = 1)
  learner$encapsulate = c(train = "evaluate", predict = "none")
  learner$fallback = lrn("classif.featureless")
  learner$train(task)

  expect_is(learner$state$fallback_state$model, "classif.featureless_model")
  expect_null(learner$state$model)
  expect_number(learner$state$train_time, lower = 0)

  expect_prediction(learner$predict(task))
})

test_that("fail during predict", {
  task = tsk("iris")
  learner = lrn("classif.debug", error_predict = 1)
  learner$encapsulate = c(predict = "evaluate")
  learner$fallback = lrn("classif.featureless")
  learner$train(task)

  expect_is(learner$state$fallback_state$model, "classif.featureless_model")
  expect_is(learner$state$model, "classif.debug_model")
  expect_number(learner$state$train_time, lower = 0)

  expect_prediction(learner$predict(task))
})

test_that("fail during resample", {
  task = tsk("iris")
  learner = lrn("classif.debug", error_predict = 1)
  learner$encapsulate = c(predict = "evaluate")
  learner$fallback = lrn("classif.featureless")

  rr = resample(tsk("iris"), learner, rsmp("cv", folds = 3))
  expect_data_table(rr$errors, nrows = 3)
  expect_number(rr$aggregate(msr("classif.ce")))
})

test_that("incomplete predictions", {
  task = tsk("iris")
  learner = lrn("classif.debug", predict_type = "prob", predict_missing = 0.5, fallback = lrn("classif.featureless"))

  learner$train(task)
  p = learner$predict(task)
  expect_prediction(p)
  expect_factor(p$response, any.missing = FALSE)
  expect_matrix(p$prob, any.missing = FALSE)

  rr = resample(tsk("iris"), learner, rsmp("cv", folds = 3))
  expect_prediction(rr$prediction())
  expect_factor(rr$prediction()$response, any.missing = FALSE)
  expect_matrix(rr$prediction()$prob, any.missing = FALSE)
})
