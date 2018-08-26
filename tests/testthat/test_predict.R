context("predict")

test_that("Simple prediction", {
  task = mlr.tasks$get("iris")
  learner = mlr.learners$get("classif.dummy")
  subset = 1:100
  e = train(task, learner, subset)$predict(subset = 101:150)$score()

  expect_r6(e, "Experiment")
  expect_false(e$has.errors)
  expect_integer(e$test.set, len = 50L, unique = TRUE, any.missing = FALSE)

  e$predict(subset = 101:150)
  expect_null(e$data$performance)
})
