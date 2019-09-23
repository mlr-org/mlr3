test_that("mlr_measures_classif.f_score", {
  task = tsk("sonar")
  lrn = lrn("classif.featureless", predict_type = "prob")
  m = msr("classif.f_score")

  pred = lrn$train(task)$predict(task)
  perf = pred$score(m)

  scores = confusion_measures(pred$confusion)
  P = scores[["ppv"]]
  R = scores[["tpr"]]

  expect_equal(2 * P * R / (P + R), unname(perf))
})
