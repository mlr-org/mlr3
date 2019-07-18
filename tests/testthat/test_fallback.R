context("fallback")

test_that("simple fallback", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_train = TRUE)
  learner$encapsulate = c(train = "evaluate")
  learner$fallback = mlr_learners$get("classif.featureless")
  learner$train(task)

  expect_is(learner$data$fallback_data$model, "classif.featureless_model")
  expect_null(learner$data$model)
  expect_number(learner$data$train_time, lower = 0)

  expect_prediction(learner$predict(task))
})
