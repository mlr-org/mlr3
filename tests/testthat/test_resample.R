context("resample")

test_that("resample", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv", param_vals = list(folds = 3L))

  rr = resample(task, learner, resampling)

  expect_resample_result(rr)
  expect_numeric(rr$performance()$classif.ce, any.missing = FALSE)
  expect_number(rr$aggregate())
  expect_different_address(rr$data$learner[[1L]], rr$data$learner[[2L]])
  expect_same_address(rr$data$task[[1L]], rr$data$task[[2L]])
  expect_same_address(rr$data$resampling[[1L]], rr$data$resampling[[2L]])

  expect_equal(uniqueN(hashes(rr$data$learner)), 1L)
  expect_equal(uniqueN(hashes(rr$data$task)), 1L)
  expect_equal(uniqueN(hashes(rr$data$resampling)), 1L)
})

test_that("resample with multiple measures", {
  task = mlr_tasks$get("iris")
  measures = mlr_measures$mget(c("classif.ce", "classif.acc"))
  learner = mlr_learners$get("classif.featureless")
  rr = resample(task, learner, "cv3")

  tab = rr$performance(measures, ids = FALSE)
  expect_data_table(tab, ncol = length(mlr_reflections$rr_names) + length(measures), nrow = 3L)
  expect_set_equal(names(tab), c(mlr_reflections$rr_names, ids(measures)))

  perf = rr$aggregate(measures)
  expect_numeric(perf, any.missing = FALSE, len = length(measures), names = "unique")
  expect_equal(names(perf), ids(measures))
})

test_that("rr$combine()", {
  task = mlr_tasks$get("iris")
  measures = mlr_measures$mget(c("classif.ce", "classif.acc"))
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

  aggr = bmr$aggregate()
  expect_data_table(aggr, nrow = 2)
  expect_set_equal(aggr$hash, c(rr1$hash, rr2$hash))
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
  expect_different_address(task, rr$task)
  expect_different_address(learner, rr$data$learner[[1L]])
  expect_different_address(resampling, rr$resampling)
})

test_that("memory footprint", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv3")
  rr = resample(task, learner, resampling)
  x = rr$data

  expect_equal(uniqueN(map_chr(x$learner, address)), nrow(x))
  expect_equal(uniqueN(map_chr(x$task, address)), 1L)
  expect_equal(uniqueN(map_chr(x$resampling, address)), 1L)
})

test_that("predict_type is checked", {
  task = mlr_tasks$get("sonar")
  learner = mlr_learners$get("classif.featureless")
  resampling = mlr_resamplings$get("cv", param_vals = list(folds = 3L))
  measure = mlr_measures$get("classif.auc")
  rr = resample(task, learner, resampling)

  expect_error(rr$performance(measure), "predict_type")
  expect_error(rr$aggregate(measure), "predict_type")
})
