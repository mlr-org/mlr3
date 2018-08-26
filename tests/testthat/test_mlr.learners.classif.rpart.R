context("mlr.learners.classif.rpart")

test_that("Simple training/predict", {
  task = mlr.tasks$get("iris")
  learner = mlr.learners$get("classif.rpart")
  expect_learner(learner, task)
})

if (FALSE) {
  task = mlr.tasks$get("iris")
  learner = mlr.learners$get("classif.rpart")
  e = Experiment$new(task = task, learner = learner)
  e$has.errors
  e$train(1:30)
  e$has.errors
  e$predict(100:110)
  e
  e$data$model
  e$predictions
}
