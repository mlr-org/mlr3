context("Measure")

test_that("Measure construction", {
  ids = mlr_measures$ids

  for (key in ids) {
    m = mlr_measures$get(key)
    expect_measure(m)
  }
})

test_that("Classification measures", {
  ids = mlr_measures$ids
  lrn = mlr_learners$get("classif.dummy")
  lrn$predict_type = "prob"
  e = Experiment$new(
    task = mlr_tasks$get("sonar"),
    learner = lrn
  )
  e$train()$predict()

  for (key in ids) {
    m = mlr_measures$get(key)
    if (is.na(m$task_type) || m$task_type == "classif") {
      perf = m$calculate(e)
      expect_number(perf, lower = m$range[1], upper = m$range[2])
    }
  }
})

test_that("Regression measures", {
  ids = mlr_measures$ids
  e = Experiment$new(
    task = mlr_tasks$get("bh"),
    learner = mlr_learners$get("regr.dummy")
  )
  e$train()$predict()

  for (key in ids) {
    m = mlr_measures$get(key)
    if (is.na(m$task_type) || m$task_type == "regr") {
      perf = m$calculate(e)
      expect_number(perf, lower = m$range[1], upper = m$range[2])
    }
  }
})
