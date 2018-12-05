context("fallback learner")

test_that("no fallback_learner, no encapsulation", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  ctrl = mlr_control(use_evaluate = FALSE)

  e = Experiment$new(task = task, learner = learner)
  expect_error(e$train(ctrl = ctrl), "crashtest")
})


test_that("fallback_learner", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  learner$fallback = mlr_learners$get("classif.featureless")
  ctrl = mlr_control(use_evaluate = TRUE)

  e = Experiment$new(task = task, learner = learner)
  e$train(ctrl = ctrl)
  e$predict(ctrl = ctrl)
  e$prediction
  e$score(ctr = ctrl)
  e$performance
})

test_that("fallback_learner, resampling", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.crashtest")
  learner$fallback = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 3L)

  ctrl = mlr_control(use_evaluate = TRUE)
  rr = resample(task, learner, resampling, ctrl = ctrl)
  expect_resample_result(rr)
  expect_true(all(rr$errors))
  expect_false(any(is.na(rr$performance(task$measures[[1L]]$id))))
  expect_number(rr$aggregated)
})
