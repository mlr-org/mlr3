context("Experiment")

test_that("Empty Experiment construction", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  e = Experiment$new()
  expect_error(e$train(), "task")
  e$task = task
  e$learner = learner
  expect_different_address(task, e$task)
  expect_different_address(learner, e$learner)
})

test_that("Partial experiments + save/restore", {
  fn = tempfile(pattern = "exp_", fileext = ".rds")
  learner = learner = mlr_learners$get("classif.rpart")

  e = Experiment$new(task = mlr_tasks$get("iris"), learner = learner)
  saveRDS(e, file = fn); e = readRDS(fn); expect_experiment(e)
  e$train(subset = 1:120)
  saveRDS(e, file = fn); e = readRDS(fn); expect_experiment(e)
  e$predict(subset = 121:150)
  saveRDS(e, file = fn); e = readRDS(fn); expect_experiment(e)
  e$score()
  saveRDS(e, file = fn); e = readRDS(fn); expect_experiment(e)
})

test_that("inputs are cloned", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")

  e = Experiment$new(task, learner)
  expect_different_address(task, e$task)
  expect_different_address(learner, e$learner)
})
