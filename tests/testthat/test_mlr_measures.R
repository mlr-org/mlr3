context("mlr_measures")

test_that("mlr_measures", {
  ids = mlr_measures$ids()

  for (key in ids) {
    m = mlr_measures$get(key)
    expect_measure(m)
  }
})

test_that("mlr_measures_auc", {
  task = mlr_tasks$get("sonar")
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  m = mlr_measures$get("classif.auc")
  task$measures = list(m)

  e = Experiment$new(task, lrn)$train()$predict()
  e$prediction$predict_types
  e$score()
  expect_equal(unname(e$performance), 0.5)
})
