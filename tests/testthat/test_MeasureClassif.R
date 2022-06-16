test_that("Classification measures", {
  keys = mlr_measures$keys("^classif\\.")
  task = tsk("sonar")
  learner = lrn("classif.rpart", predict_type = "prob")
  learner$train(task)
  p = learner$predict(task)

  for (key in keys) {
    m = mlr_measures$get(key)
    if (is.na(m$measure_type) || m$measure_type == "classif") {
      if (key == "classif.costs") {
        costs = 1 - diag(length(task$class_names))
        rownames(costs) = colnames(costs) = task$class_names
        m$costs = costs
      }
      perf = m$score(prediction = p, task = task, learner = learner)
      expect_number(perf, na.ok = FALSE, lower = m$range[1], upper = m$range[2])
    }
  }
})
