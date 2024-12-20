disable_encapsulation = function(learner) {
  learner$encapsulate("none")
  learner
}

enable_encapsulation = function(learner) {
  learner$encapsulate("evaluate", default_fallback(learner))
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
  expect_match(log$msg, "->train()", fixed = TRUE)
  expect_true("output" %in% log$class)
  expect_true("warning" %in% log$class)
  expect_false("error" %in% log$class)

  expect_message(expect_warning(disable_encapsulation(learner)$predict(task, row_ids = 101:150)))
  log = learner$log[stage == "predict"]
  expect_data_table(log)
  expect_equal(nrow(log), 0)

  p = enable_encapsulation(learner)$predict(task, row_ids = 101:150)
  log = learner$log[stage == "predict"]
  expect_data_table(log)
  expect_data_table(log, nrows = 2L, ncols = 3L, any.missing = FALSE)
  expect_factor(log$class)
  expect_equal(as.character(log$class), c("output", "warning"))
  expect_match(log$msg, "->predict()",  fixed = TRUE)
})

test_that("evaluate / resample", {
  resampling = rsmp("cv", folds = 3)

  rr = suppressMessages(suppressWarnings(resample(task, disable_encapsulation(learner), resampling)))
  expect_true(every(get_private(rr)$.data$data$fact$learner_state, function(x) nrow(x$log) == 0L))

  expect_silent(rr <- resample(task, enable_encapsulation(learner), resampling))
  expect_true(every(get_private(rr)$.data$data$fact$learner_state, function(x) all(table(x$log$stage) == 2)))
})

test_that("errors and warnings are printed with logger", {
  task = tsk("spam")

  old_threshold = lg$threshold
  lg$set_threshold("warn")
  on.exit({
    lg$set_threshold(old_threshold)
  })

  learner = enable_encapsulation(lrn("classif.debug", error_train = 1))
  expect_output(learner$train(task), "ERROR")

  learner = disable_encapsulation(lrn("classif.debug", error_train = 1))
  expect_error(learner$train(task))

  learner = enable_encapsulation(lrn("classif.debug", warning_train = 1))
  expect_output(learner$train(task), "WARN")

  learner = disable_encapsulation(lrn("classif.debug", warning_train = 1))
  expect_warning(learner$train(task))
})

test_that("encapsulate methods produce the same results", {
  rng_state = .GlobalEnv$.Random.seed
  on.exit({.GlobalEnv$.Random.seed = rng_state})

  set.seed(123)
  learner = lrn("classif.debug")
  learner$train(task)
  expect_equal(learner$model$random_number, 2986)
  expect_equal(sample(seq(1000), 1), 818)
  rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)
  expect_equal(map_int(rr$learners, function(learner) learner$model$random_number), c(37151, 94567, 21057))


  set.seed(123)
  learner = lrn("classif.debug")
  learner$encapsulate("try", lrn("classif.featureless"))
  learner$train(task)
  expect_equal(learner$model$random_number, 2986)
  expect_equal(sample(seq(1000), 1), 818)
  rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)
  expect_equal(map_int(rr$learners, function(learner) learner$model$random_number), c(37151, 94567, 21057))

  set.seed(123)
  learner = lrn("classif.debug")
  learner$encapsulate("evaluate", lrn("classif.featureless"))
  learner$train(task)
  expect_equal(learner$model$random_number, 2986)
  expect_equal(sample(seq(1000), 1), 818)
  rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)
  expect_equal(map_int(rr$learners, function(learner) learner$model$random_number), c(37151, 94567, 21057))

  set.seed(123)
  learner = lrn("classif.debug")
  learner$encapsulate("callr", lrn("classif.featureless"))
  learner$train(task)
  expect_equal(learner$model$random_number, 2986)
  expect_equal(sample(seq(1000), 1), 818)
  rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)
  expect_equal(map_int(rr$learners, function(learner) learner$model$random_number), c(37151, 94567, 21057))
})

