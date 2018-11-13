context("resample")

test_that("resample", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.dummy")
  resampling = mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 3)

  rr = resample(task, learner, resampling)

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
  resampling$param_vals = list(folds = 3)
  rr = resample(task, learner, resampling)

  expect_resample_result(rr)
})

test_that("rr$combine()", {
  task = mlr_tasks$get("iris")
  task$measures = mlr_measures$mget(c("mmce", "acc"))
  learner = mlr_learners$get("classif.dummy")
  resampling = mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 3)
  rr1 = resample(task, learner, resampling)

  learner = mlr_learners$get("classif.rpart")
  rr2 = resample(task, learner, resampling)

  bmr = rr1$combine(rr2)
  expect_benchmark_result(bmr)
  expect_equal(nrow(bmr$data), nrow(rr1$data) + nrow(rr2$data))
  expect_set_equal(bmr$data$hash, c(rr1$hash, rr2$hash))

  rrs = bmr$resample_results
  expect_data_table(rrs, nrow = 2)
  expect_set_equal(rrs$hash, c(rr1$hash, rr2$hash))
})

test_that("discarding model", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.dummy")
  resampling = mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 3)

  rr = resample(task, learner, resampling, ctrl = exec_control(store_model = FALSE))
  expect_equal(rr$data$model, vector("list", 3L))
})
