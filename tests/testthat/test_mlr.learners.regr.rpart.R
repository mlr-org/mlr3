context("mlr_learners.regr.rpart")

test_that("Simple training/predict", {
  task = mlr_tasks$get("bh")
  learner = mlr_learners$get("regr.rpart")
  expect_learner(learner, task)
})

if (FALSE) {
  e = Experiment$new(task = task, learner = learner)
  e$has_errors
  e$train(1:30)
  e$has_errors
  e$predict(100:110)
  e
  e$data$model
  e$predictions
}
