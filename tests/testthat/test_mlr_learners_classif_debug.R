context("mlr_learners_classif.debug")

test_that("Simple training/predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  expect_learner(learner, task)

  e = Experiment$new(task, learner)
  e$train()$predict()$score()
  expect_class(e$model, "unittest")
  expect_character(e$model, len = 1L, any.missing = FALSE)
  expect_factor(e$prediction$response, any.missing = FALSE, levels = levels(e$model))

  learner = mlr_learners$get("classif.debug", param_vals = list(save_tasks = TRUE))
  e = Experiment$new(task, learner)
  e$train(row_ids = 1:10)
  expect_null(e$data$learner$model)
  e$predict(row_ids = 11:20)
  expect_null(e$data$learner$model)
  model = e$model

  itrain = task$clone(TRUE)$filter(1:10)
  ipredict = task$clone(TRUE)$filter(11:20)

  expect_equal(hashes(model), hashes(list(itrain, ipredict)))
})
