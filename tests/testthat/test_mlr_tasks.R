context("mlr_tasks")

test_that("mlr_tasks", {
  ids = mlr_tasks$ids()

  for (key in ids) {
    t = mlr_tasks$get(key)
    expect_task_supervised(t)
  }
})
