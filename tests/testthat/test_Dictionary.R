context("Dictionary")

test_that("Dictionary: clone works", {
  t1 = mlr_tasks$get("iris")
  expect_task(t1)
  t2 = mlr_tasks$get("iris")
  expect_task(t2)
  expect_different_address(t1, t2)
})

test_that("$keys(pattern) works", {
  expect_subset(mlr_learners$keys("classif"), mlr_learners$keys(), empty.ok = FALSE)
})

test_that("dictionary_cast", {
  expect_list(dictionary_cast(mlr_tasks, mlr_tasks$get("iris"), "Task", FALSE), len = 1L, types = "Task")
  expect_list(dictionary_cast(mlr_tasks, list(mlr_tasks$get("iris")), "Task", FALSE), len = 1L, types = "Task")
  expect_list(dictionary_cast(mlr_tasks, "iris", "Task", FALSE), len = 1L, types = "Task")
  expect_list(dictionary_cast(mlr_tasks, c("iris", "mtcars"), "Task"), len = 2L, types = "Task")
  expect_list(dictionary_cast(mlr_tasks, list(mlr_tasks$get("mtcars"), "iris"), "Task"), len = 2L, types = "Task")
  expect_error(dictionary_cast(mlr_tasks, c("iris", "mtcars"), "Task", multiple = FALSE))
  expect_error(dictionary_cast(mlr_tasks, "key_does_not_exist", "Task", multiple = FALSE))
  expect_error(dictionary_cast(mlr_tasks, iris, "Task", multiple = FALSE))

  task = mlr_tasks$get("iris")

  x = dictionary_cast(mlr_tasks, list(task), "Task", clone = TRUE)[[1L]]
  expect_different_address(task, x)

  x = dictionary_cast(mlr_tasks, task, "Task", clone = TRUE)[[1L]]
  expect_different_address(task, x)

  x = dictionary_cast(mlr_tasks, task, "Task", clone = FALSE)[[1L]]
  expect_same_address(task, x)

  x = dictionary_cast(mlr_tasks, list(task), "Task", clone = FALSE)[[1L]]
  expect_same_address(task, x)
})
