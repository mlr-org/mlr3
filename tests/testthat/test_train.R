context("train")

test_that("Simple training", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.dummy")
  subset = 1:100
  tr = train(task, learner, subset)
  expect_r6(tr, "Experiment")
  expect_integer(tr$train_set, len = 100L, unique = TRUE, any.missing = FALSE)
})
