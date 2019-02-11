context("TaskGenerator")

test_that("TaskGenerators", {
  keys = mlr_task_generators$keys()
  n = 10L

  for (key in keys) {
    gen = mlr_task_generators$get(key)
    expect_r6(gen, "TaskGenerator", private = ".generate")
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
