context("mlr_learners_regr_dummy")

test_that("Simple training/predict", {
  task = mlr_tasks$get("bh")
  learner = mlr_learners$get("regr.dummy")
  expect_learner(learner, task)

  e = Experiment$new(task, learner)
  e$train()
  expect_class(e$model, "dummy.model")
  expect_numeric(e$model, len = 2L, any.missing = FALSE)
  e$predict()
  e$data$prediction
  e$prediction

  expect_numeric(e$prediction$response, any.missing = FALSE, len = length(e$test_set))
  e$score()
  expect_number(e$performance)
})

test_that("Predict with se", {
  task = mlr_tasks$get("bh")
  learner = mlr_learners$get("regr.dummy")
  learner$predict_type = "se"
  expect_learner(learner, task)

  e = Experiment$new(task, learner)$train()$predict()
  expect_numeric(e$data$prediction$se, len = task$nrow, any.missing = FALSE, lower = 0)
})
