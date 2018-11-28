context("Measure")

test_that("Measure construction", {
  ids = mlr_measures$keys()

  for (key in ids) {
    m = mlr_measures$get(key)
    expect_measure(m)
  }
})

test_that("Classification measures", {
  ids = mlr_measures$keys()
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  e = Experiment$new(
    task = mlr_tasks$get("sonar"),
    learner = lrn
  )
  # e$train()$predict()$score()
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
  ids = mlr_measures$keys()
  e = Experiment$new(
    task = mlr_tasks$get("bh"),
    learner = mlr_learners$get("regr.featureless")
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

test_that("Measure assertion", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  m = mlr_measures$get("time_train")
  expect_class(assert_measure(m), "Measure")
  expect_class(assert_measure(m, task = task, learner = learner), "Measure")
})
