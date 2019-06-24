context("Dictionary")

test_that("Dictionary: clone works", {
  t1 = mlr_tasks$get("iris")
  expect_task(t1)
  t2 = mlr_tasks$get("iris")
  expect_task(t2)
  expect_different_address(t1, t2)
})

test_that("$keys(pattern) works", {
  expect_subset(mlr_learners$keys("classif"), mlr_learners$keys(), empty.ok = FALSE)
})

test_that("Dictionaries are populated", {

  expect_dictionary(mlr_tasks, "Task", min_items = 1L)
  expect_dictionary(mlr_learners, "Learner", min_items = 1L)
  expect_dictionary(mlr_generators, "Generator", min_items = 1L)
  expect_dictionary(mlr_resamplings, "Resampling", min_items = 1L)
  expect_dictionary(mlr_measures, "Measure", min_items = 1L)

  expect_data_table(as.data.table(mlr_tasks), nrow = length(mlr_tasks$keys()), min.cols = 2L)
  expect_data_table(as.data.table(mlr_generators), nrow = length(mlr_generators$keys()), min.cols = 2L)
  expect_data_table(as.data.table(mlr_learners), nrow = length(mlr_learners$keys()), min.cols = 2L)
  expect_data_table(as.data.table(mlr_resamplings), nrow = length(mlr_resamplings$keys()), min.cols = 2L)
  expect_data_table(as.data.table(mlr_measures), nrow = length(mlr_measures$keys()), min.cols = 2L)

  expect_true("classif.rpart" %in% mlr_learners$keys())
  mlr_learners$remove("classif.rpart")
  expect_false("classif.rpart" %in% mlr_learners$keys())
  populate_dictionaries()
  expect_true("classif.rpart" %in% mlr_learners$keys())
})

test_that("Error when a package containing the dataset is not installed", {
  test_task = DictionaryTask$new()
  test_task$add("missing_package", function() {
    b = DataBackendDataTableVirtualKey$new(data = load_dataset("xxx", "missing_package_123"))
    TaskClassif$new("missing_package", b, target = "x", positive = "y")
  })
  expect_error(test_task$get("missing_package"))
})
