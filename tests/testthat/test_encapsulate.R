disable_encapsulation = function(learner) {
  learner$encapsulate = c(train = "none", predict = "none")
  learner
}

enable_encapsulation = function(learner) {
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")
  learner
}

task = tsk("iris")
learner = lrn("classif.debug")
learner$param_set$values = list(message_train = 1, warning_train = 1, message_predict = 1, warning_predict = 1)

test_that("evaluate / single step", {
  row_ids = 1:120
  expect_message(expect_warning(disable_encapsulation(learner)$train(task, row_ids)))
  log = learner$log
  expect_data_table(log)

  expect_silent(enable_encapsulation(learner)$train(task, row_ids))
  log = learner$log
  expect_data_table(log)
  expect_data_table(log, nrows = 2L, ncols = 3L, any.missing = FALSE)
  expect_factor(log$class)
  expect_set_equal(as.character(log$class), c("output", "warning"))
  expect_true(all(grepl("->train()", log$msg, fixed = TRUE)))
  expect_true("output" %in% log$class)
  expect_true("warning" %in% log$class)
  expect_false("error" %in% log$class)

  expect_message(expect_warning(disable_encapsulation(learner)$predict(task, row_ids = 101:150)))
  log = learner$log[stage == "predict"]
  expect_data_table(log)
  expect_equal(nrow(log), 0)

  enable_encapsulation(learner)$predict(task, row_ids = 101:150)
  log = learner$log[stage == "predict"]
  expect_data_table(log)
  expect_data_table(log, nrows = 2L, ncols = 3L, any.missing = FALSE)
  expect_factor(log$class)
  expect_equal(as.character(log$class), c("output", "warning"))
  expect_true(all(grepl("->predict()", log$msg, fixed = TRUE)))
})

test_that("evaluate / resample", {
  resampling = rsmp("cv", folds = 3)

  rr = expect_warning(resample(task, disable_encapsulation(learner), resampling))
  expect_true(all(map(rr$data$data$fact$learner_state, function(x) nrow(x$log)) == 0L))

  rr = expect_silent(resample(task, enable_encapsulation(learner), resampling))
  expect_true(all(map_lgl(rr$data$data$fact$learner_state, function(x) all(table(x$log$stage) == 2))))
})
