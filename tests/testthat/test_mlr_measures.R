context("mlr_measures")

test_that("mlr_measures", {
  ids = mlr_measures$keys()

  for (key in ids) {
    m = mlr_measures$get(key)
    expect_measure(m)
  }
})

test_that("mlr_measures_auc", {
  task = mlr_tasks$get("sonar")
  lrn = mlr_learners$get("classif.featureless")
  lrn$predict_type = "prob"
  m = mlr_measures$get("auc")

  e = Experiment$new(task, lrn)$train()$predict()
  e$score(measures = list(m))
  expect_equal(unname(e$performance), 0.5)
})
