task = tsk("iris")
learner = lrn("classif.featureless")
resampling = rsmp("cv", folds = 3)
rr = resample(task, learner, resampling)

test_that("resample", {
  expect_resample_result(rr)

  scores = rr$score(msr("classif.ce"))
  expect_list(scores$prediction, "Prediction")
  expect_numeric(scores$classif.ce, any.missing = FALSE)
  expect_number(rr$aggregate(msr("classif.ce")))
  learners = rr$learners
  expect_different_address(learners[[1L]], learners[[2L]])
  expect_equal(uniqueN(hashes(learners)), 1L)

  rr$filter(2:3)
  tab = as.data.table(rr)
  expect_data_table(tab, nrows = 2L)
  expect_data_table(tab, nrows = 2L)
  expect_equal(tab$iteration, 2:3)
  expect_resample_result(rr, allow_incomplete = TRUE)
})

test_that("empty RR", {
  rr = ResampleResult$new()
  expect_resample_result(rr)
})

test_that("resample with no or multiple measures", {
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
  measures = list(msr("classif.ce"), msr("classif.acc"))
  bmr = as_benchmark_result(rr)
  expect_benchmark_result(bmr)
  expect_equal(nrow(bmr$data), nrow(rr$data))
  expect_set_equal(bmr$uhashes, rr$uhash)
  aggr = bmr$aggregate()
  expect_data_table(aggr, nrows = 1)
  expect_set_equal(bmr$uhashes, rr$uhash)
})


test_that("discarding model", {
  expect_equal(map(as.data.table(rr)$learner, "model"), vector("list", 3L))
})

test_that("inputs are cloned", {
  expect_different_address(task, rr$data$data$tasks$task[[1]])
  expect_different_address(learner, rr$data$data$learners$learner[[1]])
  expect_different_address(resampling, rr$data$data$resamplings$resampling[[1]])
})

test_that("memory footprint", {
  expect_equal(nrow(rr$data$data$learners), 1L)
  expect_equal(nrow(rr$data$data$tasks), 1L)
  expect_equal(nrow(rr$data$data$resamplings), 1L)
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
  rr2 = with_seed(123, with_future(future::plan("multisession"), resample(task, learner, resampling)))

  expect_equal(
    as.data.table(rr1$prediction())$prob.M,
    as.data.table(rr2$prediction())$prob.M
  )
})

test_that("empty train/predict sets", {
  task = tsk("mtcars")
  learner = lrn("regr.rpart")

  expect_error(learner$train(task, integer()))

  learner$train(task)
  expect_prediction(learner$predict(task, integer()))

  resampling = rsmp("holdout", ratio = 0)
  expect_error(resample(task, learner, resampling))

  resampling = rsmp("holdout", ratio = 1)
  expect_prediction(resample(task, learner, resampling)$predictions()[[1]])
})

test_that("conditions are returned", {
  expect_true(all(c("warnings", "errors") %in% names(rr$score(conditions = TRUE))))
})
