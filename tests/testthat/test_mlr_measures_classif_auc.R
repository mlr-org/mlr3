test_that("mlr_measures_auc", {
  task = tsk("sonar")
  lrn = lrn("classif.featureless", predict_type = "prob")
  m = msr("classif.auc")
  measures = list(m)

  p = lrn$train(task)$predict(task)
  perf = p$score(measures)
  expect_equal(unname(perf), 0.5)
})
