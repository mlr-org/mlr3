context("TaskSupervised")

test_that("TaskSupervised Construction", {
  task = TaskSupervised$new(id = "foo", iris, target = "Species")
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_task_supervised(task)
})
