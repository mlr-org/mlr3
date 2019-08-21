context("mlr_measures_selected_features")

test_that("selected_features", {
  task = tsk("sonar")
  lrn = lrn("classif.rpart")
  measures = list(msr("classif.ce"), msr("selected_features"))

  rr = resample(task, lrn, rsmp("holdout"), store_models = TRUE)
  perf = rr$aggregate(measures)
  expect_count(perf[["selected_features"]])
})
