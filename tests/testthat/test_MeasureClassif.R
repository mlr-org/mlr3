context("MeasureClassif")

test_that("Classification measures", {
  ids = mlr_measures$ids()
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
