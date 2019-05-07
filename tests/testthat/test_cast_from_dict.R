context("cast_from_dict")

test_that("cast_from_dict", {
  expect_list(cast_from_dict(mlr_tasks$get("iris"), "Task", mlr_tasks, FALSE), len = 1L, types = "Task")
  expect_list(cast_from_dict(list(mlr_tasks$get("iris")), "Task", mlr_tasks, FALSE), len = 1L, types = "Task")
  expect_list(cast_from_dict("iris", "Task", mlr_tasks, FALSE), len = 1L, types = "Task")
  expect_list(cast_from_dict(c("iris", "mtcars"), "Task", mlr_tasks ), len = 2L, types = "Task")
  expect_list(cast_from_dict(list("iris", mlr_tasks$get("mtcars")), "Task", mlr_tasks), len = 2L, types = "Task")
  expect_error(cast_from_dict(c("iris", "mtcars"), "Task", mlr_tasks, multiple = FALSE))
  expect_error(cast_from_dict("key_does_not_exist", "Task", mlr_tasks, multiple = FALSE))
  expect_error(cast_from_dict(iris, "Task", mlr_tasks, multiple = FALSE))


  task = mlr_tasks$get("iris")

  x = cast_from_dict(list(task), "Task", mlr_tasks, clone = TRUE)[[1L]]
  expect_different_address(task, x)

  x = cast_from_dict(task, "Task", mlr_tasks, clone = TRUE)[[1L]]
  expect_different_address(task, x)

  x = cast_from_dict(task, "Task", mlr_tasks, clone = FALSE)[[1L]]
  expect_same_address(task, x)

  x = cast_from_dict(list(task), "Task", mlr_tasks, clone = FALSE)[[1L]]
  expect_same_address(task, x)
})
