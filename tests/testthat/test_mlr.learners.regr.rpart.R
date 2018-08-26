context("mlr.learners.regr.rpart")

test_that("Simple training/predict", {
  task = mlr.tasks$get("bh")
  learner = mlr.learners$get("regr.rpart")
  expect_learner(learner, task)
})

if (FALSE) {
  e = Experiment$new(task = task, learner = learner)
  e$has.errors
  e$train(1:30)
  e$has.errors
  e$predict(100:110)
  e
  e$data$model
  e$predictions
}
