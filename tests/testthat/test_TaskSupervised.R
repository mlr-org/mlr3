test_that("TaskSupervised Construction", {
  b = as_data_backend(iris)
  task = TaskSupervised$new(id = "foo", "classif", b, target = "Species")
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_task_supervised(task)
})
