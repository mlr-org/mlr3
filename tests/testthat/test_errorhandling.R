context("error handling")

test_that("no encapsulation", {
  task = tsk("iris")
  learner = lrn("classif.debug")

  learner$param_set$values = list(error_train = 1)
  expect_error(learner$train(task), "classif.debug->train")

  learner$param_set$values = list(error_predict = 1)
  learner$train(task)
  expect_error(learner$predict(task), "classif.debug->predict")
})

test_that("no encapsulation / resampling", {
  learner = lrn("classif.debug", error_train = 1)
  expect_error(resample(tsk("iris"), learner, rsmp("cv", folds = 3)), "'classif.debug'")
})


test_that("encapsulation", {
  task = tsk("iris")
  learner = lrn("classif.debug")
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")

  learner$param_set$values = list(warning_train = 1)
  learner$train(task)
  expect_data_table(learner$log, min.rows = 1L)
  expect_character(learner$warnings, len = 1L, any.missing = FALSE)
  expect_character(learner$errors, len = 0L, any.missing = FALSE)

  learner$param_set$values = list(error_train = 1)
  learner$train(task)
  expect_data_table(learner$log, min.rows = 1L)
  expect_character(learner$warnings, len = 0L, any.missing = FALSE)
  expect_character(learner$errors, len = 1L, any.missing = FALSE)

  learner$param_set$values = list(warning_predict = 1)
  learner$train(task)
  expect_data_table(learner$log, nrows = 0L)
  expect_character(learner$warnings, len = 0L, any.missing = FALSE)
  expect_character(learner$errors, len = 0L, any.missing = FALSE)
  expect_prediction(learner$predict(task))
  expect_character(learner$warnings, len = 1L, any.missing = FALSE)
  expect_character(learner$errors, len = 0L, any.missing = FALSE)

  learner$param_set$values = list(error_predict = 1)
  learner$train(task)
  expect_data_table(learner$log, nrows = 0L)
  expect_character(learner$warnings, len = 0L, any.missing = FALSE)
  expect_character(learner$errors, len = 0L, any.missing = FALSE)
  expect_null(learner$predict(task))
  expect_character(learner$warnings, len = 0L, any.missing = FALSE)
  expect_character(learner$errors, len = 1L, any.missing = FALSE)
})


test_that("encapsulation / resample", {
  task = tsk("iris")
  learner = lrn("classif.debug")
  learner$param_set$values = list(warning_train = 1)
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")

  rr = resample(task, learner, rsmp("cv", folds = 3))
  expect_data_table(rr$warnings, nrows = 3L)
  expect_data_table(rr$errors, nrows = 0L)

  learner$param_set$values = list(warning_train = 1, error_predict = 1)
  rr = resample(task, learner, rsmp("cv", folds = 3))
  expect_data_table(rr$warnings, nrows = 3L)
  expect_data_table(rr$errors, nrows = 3L)

  expect_equal(unname(rr$aggregate(msr("classif.ce"))), NA_real_)
  expect_equal(rr$performance(msr("classif.ce"))$classif.ce, rep(NA_real_, 3L))

  rr$performance()
})

test_that("encapsulation / benchmark", {
  task = tsk("iris")
  learner = lrn("classif.debug")
  learner$param_set$values = list(warning_train = 1)
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")

  bmr = benchmark(benchmark_grid(task, learner, rsmp("cv", folds = 3)))
  aggr = bmr$aggregate(warnings = TRUE, errors = TRUE)
  expect_equal(aggr$warnings, 3L)
  expect_equal(aggr$errors, 0L)

  learner$param_set$values = list(warning_train = 1, error_predict = 1)
  bmr = benchmark(benchmark_grid(task, learner, rsmp("cv", folds = 3)))
  aggr = bmr$aggregate(warnings = TRUE, errors = TRUE)
  expect_equal(aggr$warnings, 3L)
  expect_equal(aggr$errors, 3L)
})
