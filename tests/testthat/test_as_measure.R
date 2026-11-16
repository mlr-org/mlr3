test_that("as_measure conversion", {
  measure = msr("classif.ce")
  converted = as_measure(measure)

  expect_class(converted, "Measure")
  expect_same_address(measure, converted)

  expect_list(as_measures(measure), types = "Measure")
  expect_list(as_measures(list(measure)), types = "Measure")

  default = as_measure(NULL, task_type = "classif")
  expect_class(default, "Measure")

  default = as_measures(NULL, task_type = "classif")
  expect_list(default, types = "Measure")
})

test_that("error when arguments are misspelled", {
  expect_error(as_measure(msr("classif.acc"), clone2 = TRUE), "Received the following")
})

test_that("as_measure clones deep", {
  measure = msr("classif.fbeta", beta = 1)
  converted = as_measure(measure, clone = TRUE)
  converted$param_set$set_values(beta = 3)
  expect_equal(measure$param_set$values$beta, 1)
})

test_that("default measures", {
  expect_error(as_measure(NULL), "task type is required")
  expect_error(as_measures(NULL, clone2 = TRUE), "Received the following")
  expect_list(default_measures(NULL), len = 0L)
  expect_error(default_measures("unsupervised"), "no default measure")
})
