context("encapsulate")

test_that("encapsulate", {
  fun1 = function(...) {
    message("foo")
    warning("bar\nfoobar")
    return(99L)
  }

  fun2 = function(...) {
    1L
  }

  for (e in list(encapsulate_callr, encapsulate_evaluate)) {
    res = e(fun1)
    log = res$log
    expect_identical(res$result, 99L)
    expect_number(res$elapsed, lower = 0)
    expect_log(log)
    expect_set_equal(as.character(log$log$class), c("output", "warning"))
    expect_true(log$log[class == "warning", grepl("\n", msg, fixed = TRUE)])
    expect_true(log$has_condition("warning"))
    expect_true(log$has_condition("output"))
    expect_false(log$has_condition("error"))

    res = e(fun2)
    log = res$log
    expect_identical(res$result, 1L)
    expect_number(res$elapsed, lower = 0)
    expect_log(log)
    expect_data_table(log$log, ncol = 2L, nrow = 0L)
  }
})



is_empty_log = function(log) { test_data_table(log$log, nrow = 0L, ncol = 2L) && test_subset(log$log$class, mlr_reflections$log_classes) }
disabled = mlr_control(encapsulate_train = "none")
enabled = mlr_control(encapsulate_train = "evaluate", encapsulate_predict = "evaluate")
task = mlr_tasks$get("iris")
learner = mlr_learners$get("classif.debug")
learner$param_set$param_vals = list(message_train = TRUE, warning_train = TRUE, message_predict = TRUE, warning_predict = TRUE)

test_that("evaluate / experiment", {
  row_ids = 1:120
  e = Experiment$new(task = task, learner = learner)

  expect_message(expect_warning(e$train(row_ids, disabled)))
  log = e$data$train_log
  expect_output(print(log), "Empty <Log>")
  expect_is(log, "Log")
  expect_true(is_empty_log(log))

  expect_silent(e$train(row_ids, enabled))
  log = e$data$train_log
  expect_is(log, "Log")
  expect_output(print(log), "<Log> with 2")
  expect_data_table(log$log, nrow = 2L, ncol = 2L, any.missing = FALSE)
  expect_character(log$log$class)
  expect_set_equal(log$log$class, c("output", "warning"))
  expect_true(all(grepl("->train()", log$log$msg, fixed = TRUE)))
  expect_true(log$has_condition("output"))
  expect_true(log$has_condition("warning"))
  expect_false(log$has_condition("error"))

  expect_message(expect_warning(e$predict(row_ids = 101:150, ctrl = disabled)))
  log = e$data$predict_log
  expect_is(log, "Log")
  expect_true(is_empty_log(log))

  e$predict(row_ids = 101:150, ctrl = enabled)
  log = e$data$predict_log
  expect_is(log, "Log")
  expect_data_table(log$log, nrow = 2L, ncol = 2L, any.missing = FALSE)
  expect_character(log$log$class)
  expect_equal(log$log$class, c("output", "warning"))
  expect_true(all(grepl("->predict()", log$log$msg, fixed = TRUE)))
})

test_that("evaluate / resample", {
  resampling = mlr_resamplings$get("cv")
  resampling$param_set$param_vals = list(folds = 3)

  rr = expect_warning(resample(task, learner, resampling, ctrl = disabled))
  expect_true(every(rr$data$train_log$log, is_empty_log))
  expect_true(every(rr$data$predict_log$log, is_empty_log))

  rr = expect_silent(resample(task, learner, resampling, ctrl = enabled))
  lapply(rr$data$train_log$log, expect_data_table, , nrow = 2L, ncol = 2L, any.missing = FALSE)
  lapply(rr$data$predict_log$log, expect_data_table, nrow = 2L, ncol = 2L, any.missing = FALSE)
})
