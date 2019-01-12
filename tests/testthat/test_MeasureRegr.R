context("MeasureRegr")


test_that("Regression measures", {
  ids = mlr_measures$ids()
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
