context("mlr_learners_classif.debug")

test_that("Simple training/predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  expect_learner(learner, task)

  e = Experiment$new(task, learner)
  e$train()$predict()$score()
  expect_class(e$model, "classif.debug_model")
  expect_character(e$model$response, len = 1L, any.missing = FALSE)
  expect_factor(e$prediction$response, any.missing = FALSE, levels = levels(e$model))
})

test_that("updating model works / Experiment", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug", param_vals = list(save_tasks = TRUE))
  e = Experiment$new(task, learner)
  e$train(row_ids = 1:10)
  expect_task(e$model$task_train)
  e$predict(row_ids = 11:20)
  expect_task(e$model$task_predict)

  itrain = task$clone(TRUE)$filter(1:10)
  ipredict = task$clone(TRUE)$filter(11:20)

  expect_equal(hashes(e$model[c("task_train", "task_predict")]), hashes(list(itrain, ipredict)))
})

test_that("updating model works / resample", {
  learner = mlr_learners$get("classif.debug", param_vals = list(save_tasks = TRUE))
  rr = resample("iris", learner, "holdout")
  e = rr$experiment(1)
  expect_list(e$model, len = 3)
})
