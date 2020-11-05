test_that("TaskUnsupervised Construction", {
  b = as_data_backend(iris[-5])
  task = TaskUnsupervised$new(id = "foo", "clust", b)
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_task_supervised(task)
})
