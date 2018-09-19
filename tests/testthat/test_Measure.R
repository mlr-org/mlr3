context("Measure")

test_that("Measure construction", {
  ids = mlr_measures$ids()

  for (key in ids) {
    m = mlr_measures$get(key)
    expect_measure(m)
  }
})

test_that("Classification measures", {
  ids = mlr_measures$ids()
  e = Experiment$new(
    task = mlr_tasks$get("iris"),
    learner = mlr_learners$get("classif.dummy")
  )
  e$train()$predict()

  for (key in ids) {
    m = mlr_measures$get(key)
    if ("classif" %in% m$task_types) {
      perf = m$calculate(e)
      expect_number(perf, lower = m$range[1], upper = m$range[2])
    }
  }
})

test_that("Regression measures", {
  ids = mlr_measures$ids()
  e = Experiment$new(
    task = mlr_tasks$get("bh"),
    learner = mlr_learners$get("regr.dummy")
  )
  e$train()$predict()

  for (key in ids) {
    m = mlr_measures$get(key)
    if ("regr" %in% m$task_types) {
      perf = m$calculate(e)
      expect_number(perf, lower = m$range[1], upper = m$range[2])
    }
  }
})
