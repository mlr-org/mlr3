context("mlr_tasks")

test_that("mlr_tasks", {
  expect_dictionary(mlr_tasks, min_items = 1L)
  keys = mlr_tasks$keys()

  for (key in keys) {
    t = tsk(key)
    expect_task_supervised(t)
  }
})

test_that("load_x", {
  ns = getNamespace("mlr3")
  nn = names(ns)
  nn = nn[startsWith(names(ns), "load_task")]

  for (fun in nn) {
    fun = get(fun, envir = ns, mode = "function")
    expect_task_supervised(fun())
  }
})
