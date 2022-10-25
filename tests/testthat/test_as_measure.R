test_that("as_measure conversion", {
  measure = msr("classif.ce")
  converted = as_measure(measure)
  cloned = as_measure(measure, clone = TRUE)

  expect_class(converted, "Measure")
  expect_same_address(measure, converted)
  expect_different_address(measure, cloned)

  expect_list(as_measures(measure), types = "Measure")
  expect_list(as_measures(list(measure)), types = "Measure")

  default = as_measure(NULL, task_type = "classif")
  expect_class(default, "Measure")

  default = as_measures(NULL, task_type = "classif")
  expect_list(default, types = "Measure")
})
