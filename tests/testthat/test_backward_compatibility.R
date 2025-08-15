skip_if(Sys.getenv("GITHUB_ACTIONS") != "true", "Not on GitHub Actions")
skip_if(!identical(Sys.getenv("BRANCH_NAME"), "release"), "Not in release branch")
github_path = Sys.getenv("GITHUB_WORKSPACE")

test_that("task classif backward compatibility", {
  task = readRDS(file.path(github_path, "tests", "testthat", "_object_snapshots", "task_classif.rds"))
  expect_task(task)
})

test_that("task regr backward compatibility", {
  task = readRDS(file.path(github_path, "tests", "testthat", "_object_snapshots", "task_regr.rds"))
  expect_task(task)
})

test_that("learner classif backward compatibility", {
  learner_classif = readRDS(file.path(github_path, "tests", "testthat", "_object_snapshots", "learner_classif.rds"))
  expect_learner(learner_classif)

  task = tsk("pima")
  pred = learner_classif$predict(task)
  expect_prediction_classif(pred, task)

  learner_classif$train(task)
  expect_learner(learner_classif)
})

test_that("learner regr backward compatibility", {
  learner_regr = readRDS(file.path(github_path, "tests", "testthat", "_object_snapshots", "learner_regr.rds"))
  expect_learner(learner_regr)

  task = tsk("mtcars")
  pred = learner_regr$predict(task)
  expect_prediction_regr(pred)

  learner_regr$train(task)
  expect_learner(learner_regr)
})

test_that("resampling backward compatibility", {
  resampling = readRDS(file.path(github_path, "tests", "testthat", "_object_snapshots", "resampling.rds"))
  expect_resampling(resampling)
})

test_that("resample result backward compatibility", {
  rr = readRDS(file.path(github_path, "tests", "testthat", "_object_snapshots", "rr.rds"))
  expect_resample_result(rr)

  score = rr$score(msr("classif.ce"))
  expect_data_table(score, nrows = 3L)
  expect_numeric(score$classif.ce, len = 3L)
})

test_that("benchmark result backward compatibility", {
  bmr = readRDS(file.path(github_path, "tests", "testthat", "_object_snapshots", "bmr.rds"))
  expect_benchmark_result(bmr)

  score = bmr$score(msr("classif.ce"))
  expect_data_table(score, nrows = 3L)
  expect_numeric(score$classif.ce, len = 3L)

  aggr = bmr$aggregate()
  expect_data_table(aggr, nrows = 1L)
  expect_numeric(aggr$classif.ce, len = 1L)
})

test_that("measure backward compatibility", {
  measure = readRDS(file.path(github_path, "tests", "testthat", "_object_snapshots", "measure.rds"))
  expect_measure(measure)

  task = tsk("pima")
  learner = lrn("classif.rpart")
  learner$train(task)
  pred = learner$predict(task)
  expect_numeric(measure$score(pred), len = 1L)
})
