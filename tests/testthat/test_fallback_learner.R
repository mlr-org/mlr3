context("fallback learner")

test_that("no fallback_learner, no encapsulation", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  ctrl = mlr_control(encapsulate_train = "none", encapsulate_predict = "none")

  learner$param_set$values = list(error_train = TRUE)
  e = Experiment$new(task = task, learner = learner)
  expect_error(e$train(ctrl = ctrl), "classif.debug->train")

  learner$param_set$values = list(error_predict = TRUE)
  e = Experiment$new(task = task, learner = learner)
  e$train(ctrl = ctrl)
  expect_error(e$predict(ctrl = ctrl), "classif.debug->predict")
})


test_that("fallback_learner; fail during train", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_train = TRUE)
  learner$fallback = mlr_learners$get("classif.featureless")
  ctrl = mlr_control(encapsulate_train = "evaluate")

  e = Experiment$new(task = task, learner = learner, ctrl = ctrl)
  assert_list(e$ctrl)
  expect_equal(e$ctrl$encapsulate_train, "evaluate")
  expect_learner(e$learner$fallback)

  e$train()
  expect_true(e$has_errors)
  expect_null(e$data$learner$model)
  expect_true(inherits(e$data$learner$fallback$model, "featureless"))
  expect_true(inherits(e$model, "featureless"))

  e$predict()
  e$score()
  expect_number(e$performance)
})

test_that("fallback_learner; fail during predict", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_predict = TRUE)
  learner$fallback = mlr_learners$get("classif.featureless")
  learner$fallback$param_set$values = list(method = "sample")
  ctrl = mlr_control(encapsulate_predict = "evaluate")

  e = Experiment$new(task = task, learner = learner, ctrl = ctrl)
  assert_list(e$ctrl)
  expect_equal(e$ctrl$encapsulate_predict, "evaluate")
  expect_learner(e$learner$fallback)

  e$train()
  expect_false(e$has_errors)
  expect_true(inherits(e$model, "unittest"))
  expect_true(inherits(e$data$learner$model, "unittest"))
  expect_true(inherits(e$data$learner$fallback$model, "featureless"))

  e$predict()
  expect_true(inherits(e$model, "unittest"))
  expect_true(inherits(e$data$learner$model, "unittest"))
  expect_true(inherits(e$data$learner$fallback$model, "featureless"))
  expect_true(e$log("predict")$has_condition("error"))
  expect_true(uniqueN(e$prediction$response) > 1L)

  e$score()
  expect_number(e$performance)
})

test_that("fallback_learner, resampling", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(error_train = TRUE)
  learner$fallback = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = 3L)

  ctrl = mlr_control(encapsulate_train = "evaluate")
  rr = resample(task, learner, resampling, ctrl = ctrl)
  expect_resample_result(rr)
  expect_true(all(rr$errors))
  expect_false(any(is.na(rr$performance(task$measures[[1L]]$id))))
  expect_number(rr$aggregated)
})
