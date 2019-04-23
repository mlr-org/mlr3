context("MeasureRegr")


test_that("Regression measures", {
  keys = mlr_measures$keys()
  e = Experiment$new(
    task = mlr_tasks$get("bh"),
    learner = mlr_learners$get("regr.featureless")
  )
  e$train()$predict()

  for (key in keys) {
    m = mlr_measures$get(key)
    if (is.na(m$task_type) || m$task_type == "regr") {
      perf = m$calculate(experiment = e)
      expect_number(perf, na.ok = m$na_score, lower = m$range[1], upper = m$range[2])
    }
  }
})
