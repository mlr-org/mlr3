test_that("as_tasks_unsupervised dispatches correctly", {
  task = as_task_unsupervised(data.frame(x = 1:3))
  expect_identical(as_tasks_unsupervised(task), list(task))
  expect_false(identical(as_tasks_unsupervised(task, clone = TRUE)[[1]], task))
})
