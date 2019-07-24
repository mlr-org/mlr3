context("fallback")

test_that("train fails gracefully", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_train = TRUE)
  expect_error(learner$train(task), "'classif.debug'")
})

test_that("predict fails gracefully", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_predict = TRUE)
  learner$train(task)
  expect_error(learner$predict(task), "'classif.debug'")
})

test_that("fail during train", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_train = TRUE)
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
  learner$param_set$values = list(error_predict = TRUE)
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
  learner$param_set$values = list(error_predict = TRUE)
  learner$encapsulate = c(predict = "evaluate")
  learner$fallback = mlr_learners$get("classif.featureless")

  rr = resample("iris", learner, "cv3")
  expect_data_table(rr$errors, nrows = 3)
  expect_number(rr$aggregate())
  expect_number(rr$aggregate())
})


test_that("incomplete predictions", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug", predict_type = "prob")
  learner$param_set$values = list(predict_missing = 0.5)
  learner$fallback = mlr_learners$get("classif.featureless")

  learner$train(task)
  p = learner$predict(task)
  expect_prediction(p)
  expect_factor(p$response, any.missing = FALSE)
  expect_matrix(p$prob, any.missing = FALSE)

  rr = resample("iris", learner, "cv3")
  expect_prediction(rr$prediction)
  expect_factor(rr$prediction$response, any.missing = FALSE)
  expect_matrix(rr$prediction$prob, any.missing = FALSE)
})
