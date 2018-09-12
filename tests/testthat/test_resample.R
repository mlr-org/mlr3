context("resample")

test_that("resample", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.dummy")
  resampling = mlr_resamplings$get("cv")
  resampling$par_vals = list(folds = 3)
  rr = resample(task, learner, resampling)

  expect_resample_result(rr)
})
