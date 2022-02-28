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

test_that("tasks are cloned", {
  if (packageVersion("mlr3misc") >= "0.9.2") {
    task = tsk("iris")
    mlr_tasks$add("foo", task)
    expect_different_address(task, tsk("foo"))
    mlr_tasks$remove("foo")
  }
})

test_that("extra_cols", {
  tab = as.data.table(mlr_tasks, extract = function(x) list(hash = x$hash))
  expect_data_table(tab)
  expect_character(tab$hash, any.missing = FALSE)
})
