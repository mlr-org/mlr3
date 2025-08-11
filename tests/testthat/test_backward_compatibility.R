test_that("task classif backward compatibility",
  task = readRDS(system.file("snapshots/task_classif.rds", package = "mlr3"))
  expect_task(task)
})

test_that("task regr backward compatibility", {
  task = readRDS(system.file("snapshots/task_regr.rds", package = "mlr3"))
  expect_task(task)
})

test_that("learner classif backward compatibility", {
  learner_classif = readRDS(system.file("snapshots/learner_classif.rds", package = "mlr3"))
  expect_learner(learner_classif)

  task = tsk("pima")
  learner_classif$train(task)
  pred = learner_classif$predict(task)
  expect_prediction_classif(pred, task)
})

test_that("learner regr backward compatibility", {
  learner_regr = readRDS(system.file("snapshots/learner_regr.rds", package = "mlr3"))
  expect_learner(learner_regr)

  task = tsk("mtcars")
  learner_regr$train(task)
  pred = learner_regr$predict(task)
  expect_prediction_regr(pred, task)
})

test_that("resampling backward compatibility", {
  resampling = readRDS(system.file("snapshots/resampling.rds", package = "mlr3"))
  expect_resampling(resampling)
})

test_that("resample result backward compatibility", {
  rr = readRDS(system.file("snapshots/rr.rds", package = "mlr3"))
  expect_resample_result(rr)

  score = rr$score(msr("classif.ce"))
  expect_data_table(score, nrows = 3L)
  expect_numeric(score$classif.ce, len = 3L)
})

test_that("benchmark result backward compatibility", {
  bmr = readRDS(system.file("snapshots/bmr.rds", package = "mlr3"))
  expect_benchmark_result(bmr)

  score = bmr$score(msr("classif.ce"))
  expect_data_table(score, nrows = 3L)
  expect_numeric(score$classif.ce, len = 3L)

  aggr = bmr$aggregate()
  expect_data_table(aggr, nrows = 1L)
  expect_numeric(aggr$classif.ce, len = 1L)
})

test_that("measure backward compatibility", {
  measure = readRDS(system.file("snapshots/measure.rds", package = "mlr3"))
  expect_measure(measure)

  task = task("pima")
  learner = lrn("classif.rpart")
  learner$train(task)
  pred = learner$predict(task)
  expect_numeric(measure$score(pred), len = 1L)
})
