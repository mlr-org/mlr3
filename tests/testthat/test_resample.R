context("resample")

test_that("resample", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3L)

  rr = resample(task, learner, resampling)

  expect_resample_result(rr)
  expect_numeric(rr$score(msr("classif.ce"))$classif.ce, any.missing = FALSE)
  expect_number(rr$aggregate(msr("classif.ce")))
  expect_different_address(rr$data$learner[[1L]], rr$data$learner[[2L]])
  expect_same_address(rr$data$task[[1L]], rr$data$task[[2L]])
  expect_same_address(rr$data$resampling[[1L]], rr$data$resampling[[2L]])

  expect_equal(uniqueN(hashes(rr$data$learner)), 1L)
  expect_equal(uniqueN(hashes(rr$data$task)), 1L)
  expect_equal(uniqueN(hashes(rr$data$resampling)), 1L)

  rr$filter(2:3)
  expect_data_table(rr$data, nrows = 2L)
  expect_resample_result(rr, allow_incomplete = TRUE)
})

test_that("resample with no or multiple measures", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  rr = resample(task, learner, rsmp("cv", folds = 3))

  for (measures in list(mlr_measures$mget(c("classif.ce", "classif.acc")), list())) {
    tab = rr$score(measures, ids = FALSE)
    expect_data_table(tab, ncols = length(mlr_reflections$rr_names) + length(measures), nrows = 3L)
    expect_set_equal(names(tab), c(mlr_reflections$rr_names, ids(measures)))
    perf = rr$aggregate(measures)
    expect_numeric(perf, any.missing = FALSE, len = length(measures), names = "unique")
    expect_equal(names(perf), unname(ids(measures)))
  }
})

test_that("as_benchmark_result.ResampleResult", {
  task = tsk("iris")
  measures = list(msr("classif.ce"), msr("classif.acc"))
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3)
  rr = resample(task, learner, resampling)
  bmr = as_benchmark_result(rr)
  expect_benchmark_result(bmr)
  expect_equal(nrow(bmr$data), nrow(rr$data))
  expect_set_equal(bmr$data$uhash, rr$uhash)
  aggr = bmr$aggregate()
  expect_data_table(aggr, nrows = 1)
  expect_set_equal(bmr$uhashes, rr$uhash)
})


test_that("discarding model", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3)

  rr = resample(task, learner, resampling)
  expect_equal(map(rr$data$learner, "model"), vector("list", 3L))
})

test_that("inputs are cloned", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("holdout")
  resampling$instantiate(task)

  rr = resample(task, learner, resampling)
  expect_different_address(task, rr$task)
  expect_different_address(learner, rr$data$learner[[1L]])
  expect_different_address(resampling, rr$resampling)
})

test_that("memory footprint", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3)
  rr = resample(task, learner, resampling)
  x = rr$data

  expect_equal(uniqueN(map_chr(x$learner, address)), nrow(x))
  expect_equal(uniqueN(map_chr(x$task, address)), 1L)
  expect_equal(uniqueN(map_chr(x$resampling, address)), 1L)
})

test_that("predict_type is checked", {
  task = tsk("sonar")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3L)
  measure = msr("classif.auc")
  rr = resample(task, learner, resampling)

  expect_error(rr$score(measure), "predict_type")
  expect_error(rr$aggregate(measure), "predict_type")
})

test_that("seeds work identical sequential/parallel", {
  skip_if_not_installed("future")
  task = tsk("sonar")
  learner = lrn("classif.debug", predict_type = "prob")
  resampling = rsmp("cv", folds = 3L)
  measure = msr("classif.auc")

  rr1 = with_seed(123, with_future(future::plan("sequential"), resample(task, learner, resampling)))
  rr2 = with_seed(123, with_future(future::plan("multiprocess"), resample(task, learner, resampling)))

  expect_equal(
    as.data.table(rr1$prediction())$prob.M,
    as.data.table(rr2$prediction())$prob.M
  )
})
