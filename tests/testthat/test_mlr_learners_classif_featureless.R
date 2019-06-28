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

  learner$train(task)
  expect_class(learner$model, "classif.featureless_model")
  expect_numeric(learner$model$tab, len = 3L, any.missing = FALSE)
  prediction = learner$predict(task)
  expect_factor(prediction$response, any.missing = FALSE, levels = levels(iris$Species))
  prediction$score("classif.ce")
  expect_number(e$performance, lower = 0.6, upper = 0.7)
})

test_that("Predict with prob", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  learner$predict_type = "prob"
  expect_learner(learner, task)

  p = learner$train(task)$predict(task)
  expect_matrix(p$prob, nrow = 150L, ncol = 3L)
  expect_names(colnames(p$prob), permutation.of = levels(iris$Species))
})
