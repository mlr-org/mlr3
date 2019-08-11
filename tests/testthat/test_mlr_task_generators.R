context("mlr_task_generators")

test_that("mlr_task_generators", {
  expect_dictionary(mlr_task_generators, min_items = 1L, contains = "TaskGenerator")
  keys = mlr_task_generators$keys()
  n = 30L

  for (key in keys) {
    gen = mlr_task_generators$get(key)
    expect_r6(gen, "TaskGenerator", private = ".generate")
    expect_choice(gen$task_type, mlr_reflections$task_types$type)
    expect_function(gen$generate, args = "n")
    expect_class(gen$param_set, "ParamSet")
    expect_list(gen$param_set$values, names = "unique")

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
