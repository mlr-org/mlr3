context("fallback learner")

test_that("no fallback_learner, no encapsulation", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.unittest")
  learner$param_vals = list(error_train = TRUE)
  ctrl = mlr_control(encapsulate_train = "none")

  e = Experiment$new(task = task, learner = learner)
  expect_error(e$train(ctrl = ctrl), "classif.unittest->train")
})


test_that("fallback_learner", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.unittest")
  learner$param_vals = list(error_train = TRUE)
  learner$fallback = mlr_learners$get("classif.featureless")
  ctrl = mlr_control(encapsulate_train = "evaluate")

  e = Experiment$new(task = task, learner = learner, ctrl = ctrl)
  assert_list(e$ctrl)
  expect_equal(e$ctrl$encapsulate_train, "evaluate")

  e$train()
  e$predict()
  e$score()
  expect_number(e$performance)
})

test_that("fallback_learner, resampling", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.unittest")
  learner$param_vals = list(error_train = TRUE)
  learner$fallback = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 3L)

  ctrl = mlr_control(encapsulate_train = "evaluate")
  rr = resample(task, learner, resampling, ctrl = ctrl)
  expect_resample_result(rr)
  expect_true(all(rr$errors))
  expect_false(any(is.na(rr$performance(task$measures[[1L]]$id))))
  expect_number(rr$aggregated)
})
