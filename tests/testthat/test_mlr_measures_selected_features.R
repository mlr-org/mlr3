context("mlr_measures_selected_features")

test_that("selected_features", {
  task = mlr_tasks$get("sonar")
  lrn = mlr_learners$get("classif.rpart")
  measures = mlr_measures$mget(c("classif.ce", "selected_features"))

  rr = resample(task, lrn, mlr_resamplings$get("holdout"), ctrl = list(store_models = TRUE))
  perf = rr$aggregate(measures)
  expect_count(perf[["selected_features"]])
})
