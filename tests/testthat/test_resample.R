context("resample")

test_that("resample", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.dummy")
  resampling = mlr_resamplings$get("cv")
  resampling$par_vals = list(folds = 3)

  rr = with_plan("sequential", { resample(task, learner, resampling) })

  expect_resample_result(rr)
  expect_number(rr$aggregated)
  expect_same_address(rr$data$learner[[1L]], rr$data$learner[[2L]])
  expect_same_address(rr$data$task[[1L]], rr$data$task[[2L]])
  expect_same_address(rr$data$resampling[[1L]], rr$data$resampling[[2L]])
  expect_different_address(rr$data$model[[1L]], rr$data$model[[2L]])
})

test_that("resample with multiple measures", {
  task = mlr_tasks$get("iris")
  task$measures = mlr_measures$mget(c("mmce", "acc"))
  learner = mlr_learners$get("classif.dummy")
  resampling = mlr_resamplings$get("cv")
  resampling$par_vals = list(folds = 3)
  rr = resample(task, learner, resampling)

  expect_resample_result(rr)
})
