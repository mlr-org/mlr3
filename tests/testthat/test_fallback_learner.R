context("fallback learner")

test_that("no fallback_learner, no encapsulation", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  ctrl = mlr_control(error_handling = "off")

  e = Experiment$new(task = task, learner = learner)
  expect_error(e$train(ctrl = ctrl), "crashtest")
})

test_that("no fallback_learner, encapsulation", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  ctrl = mlr_control(error_handling = "catch")

  e = Experiment$new(task = task, learner = learner)
  e$train(ctrl = ctrl)
  e$predict(ctrl = ctrl)
  expect_error(e$prediction, "predictions available")
  e$score(ctrl = ctrl)
  expect_true(is.na(e$performance))
})

test_that("fallback_learner", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  ctrl = mlr_control(fallback_learner = mlr_learners$get("classif.featureless"), error_handling = "fallback")

  e = Experiment$new(task = task, learner = learner)
  e$train(ctrl = ctrl)
  e$predict(ctrl = ctrl)
  e$prediction
  e$score(ctr = ctrl)
  e$performance
})


test_that("no fallback_learner, resampling", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  resampling = mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 3L)

  ctrl = mlr_control(error_handling = "catch")
  rr = resample(task, learner, resampling, ctrl = ctrl)
  expect_resample_result(rr)
  expect_true(all(rr$errors))
  expect_true(all(is.na(rr$performance(task$measures[[1L]]$id))))
  expect_true(is_scalar_na(rr$aggregated))
})

test_that("fallback_learner, resampling", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  resampling = mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 3L)

  ctrl = mlr_control(error_handling = "fallback", fallback_learner = mlr_learners$get("classif.featureless"))
  rr = resample(task, learner, resampling, ctrl = ctrl)
  expect_resample_result(rr)
  expect_true(all(rr$errors))
  expect_false(any(is.na(rr$performance(task$measures[[1L]]$id))))
  expect_number(rr$aggregated)
})
