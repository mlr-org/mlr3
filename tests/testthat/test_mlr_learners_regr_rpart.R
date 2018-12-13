context("mlr_learners_regr_rpart")

test_that("Simple training/predict", {
  task = mlr_tasks$get("bh")
  learner = mlr_learners$get("regr.rpart")
  expect_learner(learner, task)
})
