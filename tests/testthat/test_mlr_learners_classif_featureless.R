context("mlr_learners_regr_featureless")

test_that("autotest", {
  learner = mlr_learners$get("classif.featureless")
  expect_learner(learner)
  result = run_autotest(learner, exclude = "sanity")
  expect_true(result, info = result$error)
})

test_that("Simple training/predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  expect_learner(learner, task)

  e = Experiment$new(task, learner)
  e$train()
  expect_class(e$model, "featureless")
  expect_numeric(e$model$tab, len = 3L, any.missing = FALSE)
  e$predict()
  expect_factor(e$prediction$response, any.missing = FALSE, levels = levels(iris$Species))
  e$score()
  expect_number(e$performance, lower = 0.6, upper = 0.7)
})

test_that("Predict with prob", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  learner$predict_type = "prob"
  expect_learner(learner, task)

  e = Experiment$new(task, learner)$train()$predict()
  expect_matrix(e$data$predicted$prob, nrow = 150L, ncol = 3L)
  expect_names(colnames(e$data$predicted$prob), permutation.of = levels(iris$Species))
})
