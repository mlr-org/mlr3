context("train")

test_that("Simple train+predict+score of featureless model", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  subset = 1:100
  e = Experiment$new(task, learner)

  e$train(subset)
  expect_set_equal(e$train_set, subset)
  expect_class(e$model, "featureless")
  expect_class(e$model, "featureless")
  expect_equal(task$nrow, 150L)
  expect_equal(e$data$task$nrow, 150L)

  e$predict(1:10)
  expect_set_equal(e$test_set, 1:10)
  expect_data_table(as.data.table(e$prediction), nrow = 10L, any.missing = FALSE)
  expect_equal(task$nrow, 150L)
  expect_equal(e$data$task$nrow, 150L)

  e$score()

  expect_experiment(e)
})
