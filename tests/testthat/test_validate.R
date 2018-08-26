context("validate")

test_that("Simple validation", {
  task = mlr.tasks$get("iris")
  task = split_train_validation(task, 0.9)
  learner = mlr.learners$get("classif.rpart")
  e = train(task, learner)
  expect_integer(e$train.set, len = task$nrow, unique = TRUE, any.missing = FALSE)
  # expect_integer(e$validation.set, len = 15L, unique = TRUE, any.missing = FALSE)
})
