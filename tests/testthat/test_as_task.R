test_that("as_task conversion", {
  task = tsk("zoo")
  converted = as_task(task)
  cloned = as_task(task, clone = TRUE)

  expect_class(converted, "Task")
  expect_same_address(task, converted)
  expect_different_address(task, cloned)

  expect_list(as_tasks(task), types = "Task")
  expect_list(as_tasks(list(task)), types = "Task")
})
