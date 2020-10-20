test_that("log to text file", {
  # See #566
  console_appender = if (packageVersion("lgr") >= "0.4.0") lg$inherited_appenders$console else lg$inherited_appenders$appenders.console
  f = tempfile("mlr3test_", fileext = "log")
  th1 = lg$threshold
  th2 = console_appender$threshold

  lg$set_threshold("debug")
  lg$add_appender(lgr::AppenderFile$new(f, threshold = "debug"), name = "testappender")
  console_appender$set_threshold("warn")

  on.exit({
    lg$remove_appender("testappender")
    lg$set_threshold(th1)
    console_appender$set_threshold(th2)
  })

  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3L)
  rr = resample(task, learner, resampling)

  lines = readLines(f)
  expect_true(any(startsWith(lines, "INFO")))
  expect_true(any(startsWith(lines, "DEBUG")))
  expect_true(any(grepl("'iris'", lines, fixed = TRUE)))
  expect_true(any(grepl("'classif.featureless'", lines, fixed = TRUE)))
})
