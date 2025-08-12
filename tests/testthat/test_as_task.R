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

test_that("as_task_xx error messages (#944)", {
  expect_error(
    as_task_classif(data.frame(x = factor(c("a", "b", "a", "b"), levels = c("a", "b")), y = factor(c("a", "b", "a", "b"), levels = c("a", "b")))),
    "argument \"target\" is missing, with no default"
  )

  expect_error(
    as_task_regr(data.frame(x = factor(c("a", "b", "a", "b"), levels = c("a", "b")), y = factor(c("a", "b", "a", "b"), levels = c("a", "b")))),
    "argument \"target\" is missing, with no default"
  )
})

test_that("error when arguments are misspelled", {
  expect_error(as_task(tsk("iris"), clone2 = TRUE), "Received the following")
})
