context("validate")

test_that("Simple validation", {
  task = mlr_tasks$get("iris")
  task = split_train_validation(task, 0.9)
  learner = mlr_learners$get("classif.rpart")
  e = Experiment$new(task = task, learner = learner)
  e$train()
  expect_integer(e$train_set, len = task$nrow, unique = TRUE, any.missing = FALSE)
  expect_integer(e$validation_set, len = 15L, unique = TRUE, any.missing = FALSE)
})
