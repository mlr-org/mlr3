context("fallback")

test_that("train fails gracefully", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_train = 1)
  expect_error(learner$train(task), "'classif.debug'")
})

test_that("predict fails gracefully", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_predict = 1)
  learner$train(task)
  expect_error(learner$predict(task), "'classif.debug'")
})

test_that("fail during train", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_train = 1)
  learner$encapsulate = c(train = "evaluate", predict = "none")
  learner$fallback = mlr_learners$get("classif.featureless")
  learner$train(task)

  expect_is(learner$data$fallback_data$model, "classif.featureless_model")
  expect_null(learner$data$model)
  expect_number(learner$data$train_time, lower = 0)

  expect_prediction(learner$predict(task))
})

test_that("fail during predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_predict = 1)
  learner$encapsulate = c(predict = "evaluate")
  learner$fallback = mlr_learners$get("classif.featureless")
  learner$train(task)

  expect_is(learner$data$fallback_data$model, "classif.featureless_model")
  expect_is(learner$data$model, "classif.debug_model")
  expect_number(learner$data$train_time, lower = 0)

  expect_prediction(learner$predict(task))
})

test_that("fail during resample", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_predict = 1)
  learner$encapsulate = c(predict = "evaluate")
  learner$fallback = mlr_learners$get("classif.featureless")

  rr = resample("iris", learner, "cv3")
  expect_data_table(rr$errors, nrows = 3)
  expect_number(rr$aggregate())
  expect_number(rr$aggregate())
})
