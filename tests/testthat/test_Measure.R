context("Measure")

test_that("assert_measure", {
  task = mlr_tasks$get("iris")
  learner = mlr_learners$get("classif.featureless")
  m = mlr_measures$get("time_train")
  expect_class(assert_measure(m), "Measure")
  expect_class(assert_measure(m, task = task), "Measure")
})
