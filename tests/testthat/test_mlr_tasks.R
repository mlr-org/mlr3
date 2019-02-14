context("mlr_tasks")

test_that("mlr_tasks", {
  keys = mlr_tasks$keys()

  for (key in keys) {
    t = mlr_tasks$get(key)
    expect_task_supervised(t)
  }
})
