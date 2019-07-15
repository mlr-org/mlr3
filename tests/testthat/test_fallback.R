context("fallback")


test_that("simple fallback", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_train = TRUE)
  learner$encapsulate = c(train = "evaluate")
  learner$fallback = mlr_learners$get("classif.featureless")
  learner$train(task)

  expect_is(learner$data$fallback_model, "classif.featureless_model")
  expect_null(learner$data$model)

  learner$predict(task)
})
