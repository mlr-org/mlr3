context("MeasureClassif")

test_that("Classification measures", {
  keys = mlr_measures$keys()
  task = mlr_tasks$get("sonar")
  learner = lrn("classif.featureless", predict_type = "prob")
  learner$train(task)
  p = learner$predict(task)

  for (key in keys) {
    m = mlr_measures$get(key)
    if (is.na(m$task_type) || m$task_type == "classif") {
      if (key == "classif.costs") {
        costs = 1 - diag(length(task$class_names))
        rownames(costs) = colnames(costs) = task$class_names
        m$costs = costs
      }
      perf = m$score(prediction = p, task = task, learner = learner)
      expect_number(perf, na.ok = m$na_score, lower = m$range[1], upper = m$range[2])
    }
  }
})
