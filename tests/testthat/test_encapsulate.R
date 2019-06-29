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
    expect_data_table(log)
    expect_set_equal(as.character(log$class), c("output", "warning"))
    expect_true(log[class == "warning", grepl("\n", msg, fixed = TRUE)])

    res = e(fun2)
    log = res$log
    expect_identical(res$result, 1L)
    expect_number(res$elapsed, lower = 0)
    expect_null(log)
  }
})



disabled = mlr_control(encapsulate_train = "none")
enabled = mlr_control(encapsulate_train = "evaluate", encapsulate_predict = "evaluate")
task = mlr_tasks$get("iris")
learner = mlr_learners$get("classif.debug")
learner$param_set$values = list(message_train = TRUE, warning_train = TRUE, message_predict = TRUE, warning_predict = TRUE)

test_that("evaluate / experiment", {
  row_ids = 1:120

  expect_message(expect_warning(learner$train(task, row_ids, ctrl = disabled)))
  log = learner$log
  expect_data_table(log)

  expect_silent(learner$train(task, row_ids, ctrl = enabled))
  log = learner$log
  expect_data_table(log)
  expect_data_table(log, nrow = 2L, ncol = 3L, any.missing = FALSE)
  expect_factor(log$class)
  expect_set_equal(as.character(log$class), c("output", "warning"))
  expect_true(all(grepl("->train()", log$msg, fixed = TRUE)))
  expect_true("output" %in% log$class)
  expect_true("warning" %in% log$class)
  expect_false("error" %in% log$class)

  expect_message(expect_warning(learner$predict(task, row_ids = 101:150, ctrl = disabled)))
  log = learner$log[stage == "predict"]
  expect_data_table(log)
  expect_equal(nrow(log), 0)

  learner$predict(task, row_ids = 101:150, ctrl = enabled)
  log = learner$log[stage == "predict"]
  expect_data_table(log)
  expect_data_table(log, nrow = 2L, ncol = 3L, any.missing = FALSE)
  expect_factor(log$class)
  expect_equal(as.character(log$class), c("output", "warning"))
  expect_true(all(grepl("->predict()", log$msg, fixed = TRUE)))
})

test_that("evaluate / resample", {
  resampling = mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = 3)

  rr = expect_warning(resample(task, learner, resampling, ctrl = disabled))
  expect_true(all(map(rr$data$learner, function(x) nrow(x$log)) == 0L))

  rr = expect_silent(resample(task, learner, resampling, ctrl = enabled))
  expect_true(all(map_lgl(rr$data$learner, function(x) all(table(x$log$stage) == 2))))
})
