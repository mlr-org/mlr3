context("Measure")

test_that("print.Measure", {
  m = mlr_measures$get("classif.ce")
  expect_output(print(Measure))
})

test_that("assert_measure", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  m = mlr_measures$get("time_train")
  expect_class(assert_measure(m), "Measure")
  expect_class(assert_measure(m, task = task), "Measure")
})
