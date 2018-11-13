context("evaluate")

is_empty_log = function(log) { test_data_table(log$messages, nrow = 0L, ncol = 2L) && test_factor(log$messages$class, levels = mlr_reflections$log_classes) }
disabled = exec_control(use_future = FALSE, use_evaluate = FALSE)
enabled = exec_control(use_future = FALSE, use_evaluate = TRUE, verbose = FALSE)
task = mlr_tasks$get("iris")
learner = mlr_learners$get("classif.verbose")
learner$param_vals = list(message = TRUE, warning = TRUE)

test_that("evaluate / experiment", {
  subset = 1:120
  e = Experiment$new(task = task, learner = learner)

  expect_message(expect_warning(e$train(subset, disabled)))
  log = e$data$train_log
  expect_output(print(log), "<Log> with 0")
  expect_is(log, "Log")
  expect_true(is_empty_log(log))

  expect_silent(e$train(subset, enabled))
  log = e$data$train_log
  expect_is(log, "Log")
  expect_output(print(log), "<Log> with 2")
  expect_data_table(log$messages, nrow = 2L, ncol = 2L, any.missing = FALSE)
  expect_factor(log$messages$class, levels = mlr_reflections$log_classes)
  expect_equal(as.character(log$messages$class), c("message", "warning"))
  expect_true(all(grepl("$train()", log$messages$msg, fixed = TRUE)))
  expect_true(log$has_condition("message"))
  expect_true(log$has_condition("warning"))
  expect_false(log$has_condition("error"))

  expect_message(expect_warning(e$predict(subset = 101:150, ctrl = disabled)))
  log = e$data$predict_log
  expect_is(log, "Log")
  expect_true(is_empty_log(log))

  e$predict(subset = 101:150, ctrl = enabled)
  log = e$data$predict_log
  expect_is(log, "Log")
  expect_data_table(log$messages, nrow = 2L, ncol = 2L, any.missing = FALSE)
  expect_factor(log$messages$class, levels = mlr_reflections$log_classes)
  expect_equal(as.character(log$messages$class), c("message", "warning"))
  expect_true(all(grepl("$predict()", log$messages$msg, fixed = TRUE)))
})

test_that("evaluate / resample", {
  resampling = mlr_resamplings$get("cv")
  resampling$param_vals = list(folds = 3)

  rr = expect_warning(resample(task, learner, resampling, ctrl = disabled))
  expect_true(all(map_lgl(rr$data$train_log$messages, is_empty_log)))
  expect_true(all(map_lgl(rr$data$predict_log$messages, is_empty_log)))

  rr = expect_silent(resample(task, learner, resampling, ctrl = enabled))
  lapply(rr$data$train_log$messages, expect_data_table, , nrow = 2L, ncol = 2L, any.missing = FALSE)
  lapply(rr$data$predict_log$messages, expect_data_table, nrow = 2L, ncol = 2L, any.missing = FALSE)
})
