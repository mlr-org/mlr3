context("MeasureRegr")

test_that("Regression measures", {
  keys = mlr_measures$keys()
  task = mlr_tasks$get("boston_housing")
  learner = mlr_learners$get("regr.featureless")
  learner$train(task)
  p = learner$predict(task)

  for (key in keys) {
    m = mlr_measures$get(key)
    if (is.na(m$task_type) || m$task_type == "regr") {
      perf = m$score(prediction = p, task = task, learner = learner)
      expect_number(perf, na.ok = m$na_score, lower = m$range[1], upper = m$range[2])
    }
  }
})
