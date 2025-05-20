test_that("log to text file", {
  # See #566
  console_appender = if (packageVersion("lgr") >= "0.4.0") lg$inherited_appenders$console else lg$inherited_appenders$appenders.console
  f = tempfile("mlr3test_", fileext = "log")
  th1 = console_appender$threshold

  lg$set_threshold("debug")
  lg$add_appender(lgr::AppenderFile$new(f, threshold = "debug"), name = "testappender")
  console_appender$set_threshold("warn")

  on.exit({
    lg$remove_appender("testappender")
    lg$set_threshold(0)
    console_appender$set_threshold(th1)
  })

  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3L)
  rr = suppressWarnings(resample(task, learner, resampling))

  lines = readLines(f)
  expect_true(any(startsWith(lines, "INFO")))
  expect_true(any(startsWith(lines, "DEBUG")))
  expect_match(lines, "'iris'", fixed = TRUE, all = FALSE)
  expect_match(lines, "'classif.featureless'", fixed = TRUE, all = FALSE)
})

test_that("logger works", {
  on.exit({
    lgr::get_logger("mlr3")$set_threshold(0L)
    lgr::get_logger("mlr3/core")$set_threshold(0L)
  })

  lgr::get_logger("mlr3")$set_threshold(NULL)
  lgr::get_logger("mlr3/core")$set_threshold(NULL)

  res = capture_output(resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L)))
  expect_match(res, "\\[mlr3\\]")

  lgr::get_logger("mlr3")$set_threshold("error")
  res = capture_output(resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L)))
  expect_equal(res, "")

  lgr::get_logger("mlr3")$set_threshold("info")
  res = capture_output(resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L)))
  expect_match(res, "\\[mlr3\\]")

  lgr::get_logger("mlr3/core")$set_threshold("error")
  res = capture_output(resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L)))
  expect_equal(res, "")

  # if the child logger is configured, it overrides the parent logger
  lgr::get_logger("mlr3")$set_threshold("info")
  res = capture_output(resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L)))
  expect_equal(res, "")
})

test_that("thresholds are restored on workers", {

  on.exit({
    lgr::get_logger("mlr3")$set_threshold(0L)
    lgr::get_logger("mlr3/core")$set_threshold(0L)
  })

  lgr::get_logger("mlr3")$set_threshold(NULL)
  lgr::get_logger("mlr3/core")$set_threshold(NULL)

  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_match(res, "\\[mlr3\\]")

  lgr::get_logger("mlr3")$set_threshold("error")

  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_equal(res, "")

  lgr::get_logger("mlr3")$set_threshold("info")
  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_match(res, "\\[mlr3\\]")

  lgr::get_logger("mlr3/core")$set_threshold("error")
  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_equal(res, "")

  # if the child logger is configured, it overrides the parent logger
  lgr::get_logger("mlr3")$set_threshold("info")
  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_equal(res, "")
})

