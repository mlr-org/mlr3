context("mlr_learners_classif.debug")

test_that("Simple training/predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  expect_learner(learner, task)

  e = Experiment$new(task, learner)
  e$train()
  expect_class(e$model, "unittest")
  expect_character(e$model, len = 1L, any.missing = FALSE)
  e$predict()
  e$data$prediction
  e$prediction

  expect_factor(e$prediction$response, any.missing = FALSE, levels = levels(e$model))
  e$score()

  e$learner$param_set$values$save_tasks = TRUE
  e$train(row_ids = 1:10)
  e$predict(row_ids = 11:20)
  model = e$model

  itrain = task$clone(TRUE)$filter(1:10)
  ipredict = task$clone(TRUE)$filter(11:20)

  expect_equal(hashes(model), hashes(list(itrain, ipredict)))
})
