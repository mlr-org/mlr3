context("predict")

test_that("Simple prediction", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.dummy")
  subset = 1:120
  e = Experiment$new(task = task, learner = learner)
  e$train(subset)
  e$predict(subset = 101:150)
  e$score()

  expect_r6(e, "Experiment")
  expect_false(e$has_errors)
  expect_integer(e$test_set, len = 50L, unique = TRUE, any.missing = FALSE)

  e$predict(subset = 101:150)
  expect_null(e$data$performance) # performance is reset?

  expect_data_table(e$prediction, nrow = 50)

  p = e$data$prediction
  expect_is(p, "Prediction")
  expect_data_frame(as.data.frame(p), nrow = 50)
})
