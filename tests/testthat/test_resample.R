context("resample")

test_that("resample", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv", param_vals = list(folds = 3L))

  rr = resample(task, learner, resampling)

  expect_resample_result(rr)
  expect_numeric(rr$performance(task$measures[[1]]$id), any.missing = FALSE)
  expect_number(rr$aggregated)
  expect_same_address(rr$data$learner[[1L]], rr$data$learner[[2L]])
  expect_same_address(rr$data$task[[1L]], rr$data$task[[2L]])
  expect_same_address(rr$data$resampling[[1L]], rr$data$resampling[[2L]])
  expect_different_address(rr$data$model[[1]], rr$data$model[[2L]])
})

test_that("resample with multiple measures", {
  task = mlr_tasks$get("iris")
  task$measures = mlr_measures$mget(c("classif.ce", "classif.acc"))
  learner = mlr_learners$get("classif.featureless")
  rr = resample(task, learner, "cv3")

  expect_resample_result(rr)
})

test_that("resample with replacement measures", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  rr = resample(task, learner, "cv3", measures = mlr_measures$mget(c("classif.ce", "classif.acc")))
  expect_equal(rr$measures$measure_id, c("classif.ce", "classif.acc"))
  expect_equal(names(rr$aggregated), c("classif.ce", "classif.acc"))
})

test_that("rr$combine()", {
  task = mlr_tasks$get("iris")
  task$measures = mlr_measures$mget(c("classif.ce", "classif.acc"))
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = 3)
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
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = 3)

  rr = resample(task, learner, resampling, ctrl = mlr_control(store_model = FALSE))
  expect_equal(map(rr$data$learner, "model"), vector("list", 3L))
})

test_that("inputs are cloned", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("holdout")
  resampling$instantiate(task)

  rr = resample(task, learner, resampling)
  e = rr$experiment(1L)
  expect_different_address(task, e$task)
  expect_different_address(learner, e$learner)
  expect_different_address(resampling, e$data$resampling)
})

test_that("memory footprint", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv3")
  rr = resample(task, learner, resampling)
  x = rr$data

  expect_equal(uniqueN(map_chr(x$learner, address)), 1L)
  expect_equal(uniqueN(map_chr(x$task, address)), 1L)
  expect_equal(uniqueN(map_chr(x$resampling, address)), 1L)
  expect_equal(uniqueN(map_chr(x$measures, address)), 1L)
})
