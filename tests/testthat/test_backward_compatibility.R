test_that("task classif backward compatibility", {
  skip_on_cran()

  task = readRDS("inst/snapshots/task_classif.rds")
  expect_task(task)
})

test_that("task regr backward compatibility", {
  skip_on_cran()

  task = readRDS("inst/snapshots/task_regr.rds")
  expect_task(task)
})

test_that("learner classif backward compatibility", {
  skip_on_cran()

  learner_classif = readRDS("inst/snapshots/learner_classif.rds")
  expect_learner(learner_classif)

  task = tsk("pima")
  learner_classif$train(task)
  pred = learner_classif$predict(task)
  expect_prediction_classif(pred, task)
})

test_that("learner regr backward compatibility", {
  skip_on_cran()

  learner_regr = readRDS("inst/snapshots/learner_regr.rds")
  expect_learner(learner_regr)

  task = tsk("mtcars")
  learner_regr$train(task)
  pred = learner_regr$predict(task)
  expect_prediction_regr(pred)
})

test_that("resampling backward compatibility", {
  resampling = readRDS(system.file("snapshots/resampling.rds", package = "mlr3"))
  expect_resampling(resampling)
})

test_that("resample result backward compatibility", {
  skip_on_cran()

  rr = readRDS("inst/snapshots/rr.rds")
  expect_resample_result(rr)

  score = rr$score(msr("classif.ce"))
  expect_data_table(score, nrows = 3L)
  expect_numeric(score$classif.ce, len = 3L)
})

test_that("benchmark result backward compatibility", {
  skip_on_cran()

  bmr = readRDS("inst/snapshots/bmr.rds")
  expect_benchmark_result(bmr)

  score = bmr$score(msr("classif.ce"))
  expect_data_table(score, nrows = 3L)
  expect_numeric(score$classif.ce, len = 3L)

  aggr = bmr$aggregate()
  expect_data_table(aggr, nrows = 1L)
  expect_numeric(aggr$classif.ce, len = 1L)
})

test_that("measure backward compatibility", {
  skip_on_cran()

  measure = readRDS("inst/snapshots/measure.rds")
  expect_measure(measure)

  task = tsk("pima")
  learner = lrn("classif.rpart")
  learner$train(task)
  pred = learner$predict(task)
  expect_numeric(measure$score(pred), len = 1L)
})
