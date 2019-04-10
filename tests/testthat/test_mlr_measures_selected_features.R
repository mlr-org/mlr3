context("mlr_measures_selected_features")

test_that("selected_features", {
  task = mlr_tasks$get("sonar")
  task$measures = mlr_measures$mget(c("classif.mmce", "selected_features"))
  lrn = mlr_learners$get("classif.rpart")

  rr = resample(task, lrn, mlr_resamplings$get("holdout"))
  expect_number(rr$performance("selected_features"))
})
