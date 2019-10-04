context("mlr_task_generators")

test_that("mlr_task_generators", {
  expect_dictionary(mlr_task_generators, min_items = 1L, contains = "TaskGenerator")
  keys = mlr_task_generators$keys()
  n = 30L

  for (key in keys) {
    gen = mlr_task_generators$get(key)
    expect_task_generator(gen)

    task = gen$generate(n)
    expect_task(task)
    expect_equal(gen$task_type, task$task_type)
    expect_equal(task$nrow, n)
  }
})

test_that("as.data.table(mlr_task_generators)", {
  tab = as.data.table(mlr_task_generators)
  expect_data_table(tab, min.cols = 2L)
  expect_character(tab$key, unique = TRUE, any.missing = FALSE)
})
