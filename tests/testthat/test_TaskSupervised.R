context("TaskSupervised")

test_that("TaskSupervised Construction", {
  b = DataBackendDataTable$new(iris)
  task = TaskSupervised$new(id = "foo", b, target = "Species")
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_task_supervised(task)
})
