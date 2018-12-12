context("parallelization")


test_that("parallel resample", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  future::plan(future.callr::callr)
  expect_true(future_remote())

  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.rpart")
  resampling = mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 3)

  rr = resample(task, learner, resampling)
  expect_resample_result(rr)
  expect_false(any(rr$errors))
})
