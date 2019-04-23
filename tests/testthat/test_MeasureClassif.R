context("MeasureClassif")

test_that("Classification measures", {
  keys = mlr_measures$keys()
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  e = Experiment$new(
    task = mlr_tasks$get("sonar"),
    learner = lrn
  )
  e$train()$predict()

  for (key in keys) {
    m = mlr_measures$get(key)
    if (is.na(m$task_type) || m$task_type == "classif") {
      if (key == "classif.costs") {
        costs = 1 - diag(e$task$class_n)
        rownames(costs) = colnames(costs) = e$task$class_names
        m$costs = costs
      }
      perf = m$calculate(experiment = e)
      expect_number(perf, na.ok = m$na_score, lower = m$range[1], upper = m$range[2])
    }
  }
})
