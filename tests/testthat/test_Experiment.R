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
  e$train(row_ids = 1:120)
  saveRDS(e, file = fn); e = readRDS(fn); expect_experiment(e)
  e$predict(row_ids = 121:150)
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

test_that("$predict()", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  row_ids = 1:120
  e = Experiment$new(task = task, learner = learner)
  e$train(row_ids)
  e$predict(row_ids = 101:150)
  e$score()

  expect_r6(e, "Experiment")
  expect_false(e$has_errors)
  expect_integer(e$test_set, len = 50L, unique = TRUE, any.missing = FALSE)

  e$predict(row_ids = 101:150)
  expect_null(e$data$performance) # performance is reset?

  expect_class(e$prediction, "Prediction")
  expect_data_table(as.data.table(e$prediction), nrow = 50)
  expect_data_frame(as.data.frame(e$prediction), nrow = 50)
})
