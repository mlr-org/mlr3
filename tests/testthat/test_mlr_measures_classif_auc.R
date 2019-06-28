test_that("mlr_measures_auc", {
  task = mlr_tasks$get("sonar")
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  m = mlr_measures$get("classif.auc")
  measures = list(m)

  p = lrn$train(task)$predict(task)
  perf = p$score(measures)
  expect_equal(unname(perf), 0.5)
})
