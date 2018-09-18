context("mlr_learners_classif_rpart")

test_that("Simple training/predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  learner$predict_type = "prob"
  e = Experiment$new(task = task, learner = learner)
  e$train()$predict()
  e$prediction

  expect_learner(learner, task)
})
