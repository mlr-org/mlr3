context("error handling")

test_that("no encapsulation", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  ctrl = mlr_control(encapsulate_train = "none", encapsulate_predict = "none")

  learner$param_set$values = list(error_train = TRUE)
  expect_error(learner$train(task, ctrl = ctrl), "classif.debug->train")

  learner$param_set$values = list(error_predict = TRUE)
  learner$train(task, ctrl = ctrl)
  expect_error(learner$predict(task, ctrl = ctrl), "classif.debug->predict")
})

test_that("encapsulation", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  ctrl = mlr_control(encapsulate_train = "evaluate", encapsulate_predict = "evaluate")

  learner$param_set$values = list(warning_train = TRUE)
  learner$train(task, ctrl = ctrl)
  expect_data_table(learner$log, min.rows = 1L)
  expect_character(learner$warnings, len = 1L, any.missing = FALSE)
  expect_character(learner$errors, len = 0L, any.missing = FALSE)

  learner$param_set$values = list(error_train = TRUE)
  learner$train(task, ctrl = ctrl)
  expect_data_table(learner$log, min.rows = 1L)
  expect_character(learner$warnings, len = 0L, any.missing = FALSE)
  expect_character(learner$errors, len = 1L, any.missing = FALSE)

  learner$param_set$values = list(warning_predict = TRUE)
  learner$train(task, ctrl = ctrl)
  expect_data_table(learner$log, nrows = 0L)
  expect_character(learner$warnings, len = 0L, any.missing = FALSE)
  expect_character(learner$errors, len = 0L, any.missing = FALSE)
  learner$predict(task, ctrl = ctrl)
  expect_character(learner$warnings, len = 1L, any.missing = FALSE)
  expect_character(learner$errors, len = 0L, any.missing = FALSE)

  learner$param_set$values = list(error_predict = TRUE)
  learner$train(task, ctrl = ctrl)
  expect_data_table(learner$log, nrows = 0L)
  expect_character(learner$warnings, len = 0L, any.missing = FALSE)
  expect_character(learner$errors, len = 0L, any.missing = FALSE)
  learner$predict(task, ctrl = ctrl)
  expect_character(learner$warnings, len = 0L, any.missing = FALSE)
  expect_character(learner$errors, len = 1L, any.missing = FALSE)
})


test_that("encapsulation / resample", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(warning_train = TRUE)
  ctrl = mlr_control(encapsulate_train = "evaluate", encapsulate_predict = "evaluate")

  rr = resample(task, learner, "cv3", ctrl)
  expect_data_table(rr$warnings, nrows = 3L)
  expect_data_table(rr$errors, nrows = 0L)

  learner$param_set$values = list(warning_train = TRUE, error_predict = TRUE)
  rr = resample(task, learner, "cv3", ctrl)
  expect_data_table(rr$warnings, nrows = 3L)
  expect_data_table(rr$errors, nrows = 3L)
})

test_that("encapsulation / benchmark", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.debug")
  learner$param_set$values = list(warning_train = TRUE)
  ctrl = mlr_control(encapsulate_train = "evaluate", encapsulate_predict = "evaluate")

  bmr = benchmark(expand_grid(task, learner, "cv3"), ctrl = ctrl)
  aggr = bmr$aggregate(warnings = TRUE, errors = TRUE)
  expect_equal(aggr$warnings, 3L)
  expect_equal(aggr$errors, 0L)

  learner$param_set$values = list(warning_train = TRUE, error_predict = TRUE)
  bmr = benchmark(expand_grid(task, learner, "cv3"), ctrl = ctrl)
  aggr = bmr$aggregate(warnings = TRUE, errors = TRUE)
  expect_equal(aggr$warnings, 3L)
  expect_equal(aggr$errors, 3L)
})
