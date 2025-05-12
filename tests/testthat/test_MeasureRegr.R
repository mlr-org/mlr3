test_that("Regression measures", {
  keys = mlr_measures$keys("^regr\\.")
  task = tsk("california_housing")
  learner = lrn("regr.rpart")
  learner$train(task)
  p = learner$predict(task)

  for (key in keys) {
    m = mlr_measures$get(key)

    if (is.na(m$task_type) || m$task_type == "regr") {
      if (m$predict_type == "quantiles") {
        learner_q = lrn("regr.featureless", predict_type = "quantiles", quantiles = 0.5)
        learner_q$train(task)
        p_q = learner_q$predict(task)
        perf = m$score(prediction = p_q, task = task, learner = learner_q)
        expect_number(perf, na.ok = FALSE, lower = m$range[1], upper = m$range[2])
      } else {
        perf = m$score(prediction = p, task = task, learner = learner)
        expect_number(perf, na.ok = FALSE, lower = m$range[1], upper = m$range[2])
      }
    }
  }
})
