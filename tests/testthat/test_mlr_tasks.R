context("mlr_tasks")

test_that("mlr_tasks", {
  keys = mlr_tasks$keys()

  for (key in keys) {
    t = mlr_tasks$get(key)
    expect_task_supervised(t)
  }
})

test_that("load_x", {
  ns = getNamespace("mlr3")
  nn = names(ns)
  nn = nn[startsWith(names(ns), "load_task")]

  for (fun in nn) {
    fun = match.fun(fun)
    expect_task_supervised(fun())
  }
})
