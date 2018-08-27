context("mlr_learners.classif.rpart")

test_that("Simple training/predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  expect_learner(learner, task)
})

if (FALSE) {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  e = Experiment$new(task = task, learner = learner)
  e$has_errors
  e$train(1:30)
  e$has_errors
  e$predict(100:110)
  e
  e$data$model
  e$predictions
}
