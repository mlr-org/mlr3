context("mlr_learners_regr_featureless")

test_that("Simple training/predict", {

  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  expect_learner(learner, task)

  e = Experiment$new(task, learner)
  e$train()
  expect_class(e$model, "unittest.model")
  expect_character(e$model, len = 1L, any.missing = FALSE)
  e$predict()
  e$data$prediction
  e$prediction

  expect_factor(e$prediction$response, any.missing = FALSE, levels = levels(e$model))
  e$score()

  e$learner$param_vals$save_tasks = TRUE

  e$train(row_ids = 1:10)
  e$predict(row_ids = 11:20)
  itrain = task$clone(deep = TRUE)
  itrain$hash
  itrain$filter(1:10)
  ipredict = task$clone(deep = TRUE)
  ipredict$hash  # FIXME: when issue 130 is fixed this line can go
  ipredict$filter(11:20)
  ipredict$hash  # FIXME: when issue 130 is fixed this line can go
  expect_equal(e$model, list(itrain, ipredict))

})

test_that("autotest", {
  learner = mlr_learners$get("classif.featureless")
  expect_autotest(learner)
})
