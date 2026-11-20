test_that("mlr_tasks", {
  skip_if_not_installed("mlbench")
  skip_if_not_installed("palmerpenguins")
  expect_dictionary(mlr_tasks, min_items = 1L)
  keys = mlr_tasks$keys()

  for (key in keys) {
    t = tsk(key)
    expect_task_supervised(t)
  }
})

test_that("load_x", {
  skip_if_not_installed("mlbench")
  skip_if_not_installed("palmerpenguins")
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

test_that("as.data.table(..., objects = TRUE)", {
  skip_if_not_installed("mlbench")
  skip_if_not_installed("palmerpenguins")
  tab = as.data.table(mlr_tasks, objects = TRUE)
  expect_data_table(tab)
  expect_list(tab$object, "Task", any.missing = FALSE)
})

test_that("as.data.table(mlr_tasks) skips tasks with missing packages", {
  mlr_tasks$add("missing_package", function() load_dataset("data", "mlr3_package_not_installed"))
  on.exit(mlr_tasks$remove("missing_package"))

  expect_error(tsk("missing_package"), class = "packageNotFoundError")
  tab = as.data.table(mlr_tasks)
  expect_data_table(tab, nrows = length(mlr_tasks$keys()))
  expect_true(is.na(tab["missing_package", task_type]))
  expect_false(anyMissing(tab[!"missing_package", task_type]))
})
