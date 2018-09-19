context("mlr_learners_regr_dummy")

test_that("Simple training/predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.dummy")
  expect_learner(learner, task)

  e = Experiment$new(task, learner)
  e$train()
  expect_class(e$model, "dummy.model")
  expect_numeric(e$model, len = 3L, any.missing = FALSE)
  e$predict()
  e$data$prediction
  e$prediction

  expect_factor(e$prediction$response, any.missing = FALSE, levels = levels(iris$Species))
  e$score()
  expect_number(e$performance, lower = 0.6, upper = 0.7)
})

test_that("Predict with prob", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.dummy")
  learner$predict_type = "prob"
  expect_learner(learner, task)

  e = Experiment$new(task, learner)$train()$predict()
  expect_matrix(e$data$prediction$prob, nrow = 150L, ncol = 3L)
  expect_names(colnames(e$data$prediction$prob), permutation.of = levels(iris$Species))
})
