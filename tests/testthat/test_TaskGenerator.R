context("Generator")

test_that("Generators", {
  keys = mlr_generators$keys()
  n = 10L

  for (key in keys) {
    gen = mlr_generators$get(key)
    expect_r6(gen, "Generator", private = ".generate")
    expect_choice(gen$task_type, mlr_reflections$task_types)
    expect_function(gen$generate, args = "n")
    expect_class(gen$param_set, "ParamSet")
    expect_list(gen$param_set$values, names = "unique")

    task = gen$generate(n)
    expect_task(task)
    expect_equal(gen$task_type, task$task_type)
    expect_equal(task$nrow, n)
  }
})

test_that("as.data.table.DictionaryGenerators", {
  tab = as.data.table(mlr_generators)
  expect_data_table(tab, min.cols = 2L)
  expect_names(names(tab), must.include = "key")
})
