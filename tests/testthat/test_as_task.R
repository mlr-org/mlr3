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
    as_task_classif(data.frame(
      x = factor(c("a", "b", "a", "b"), levels = c("a", "b")),
      y = factor(c("a", "b", "a", "b"), levels = c("a", "b"))
    )),
    "argument \"target\" is missing, with no default"
  )

  expect_error(
    as_task_regr(data.frame(
      x = factor(c("a", "b", "a", "b"), levels = c("a", "b")),
      y = factor(c("a", "b", "a", "b"), levels = c("a", "b"))
    )),
    "argument \"target\" is missing, with no default"
  )
})

test_that("error when arguments are misspelled", {
  expect_error(as_task(tsk("iris"), clone2 = TRUE), "Received the following")
  expect_error(as_task_classif(iris, target = "Species", positve = "setosa"), "Received the following")
  expect_error(as_task_regr(mtcars, target = "mpg", lable = "cars"), "Received the following")
  expect_error(as_task_regr(mpg ~ ., mtcars, lable = "cars"), "Received the following")
  expect_error(as_task_classif(as.matrix(iris), target = "Species", lable = "iris"), "Received the following")
  data = data.frame(y = c("no", "yes", "no"), x = 1:3)
  expect_identical(as_task_classif(as.matrix(data), target = "y", positive = "yes")$positive, "yes")
})

test_that("typed converters return a deep clone", {
  check = function(task, converter) {
    task$internal_valid_task = 1:20
    cloned = converter(task, clone = TRUE)
    expect_different_address(task, cloned)
    cloned$internal_valid_task$filter(1:5)
    expect_equal(task$internal_valid_task$nrow, 20L)
  }

  check(tsk("iris"), as_task_classif)
  check(tsk("mtcars"), as_task_regr)
  check(as_task_unsupervised(iris), as_task_unsupervised)
  check(as_task_unsupervised(iris), function(x, clone) as_tasks_unsupervised(x, clone = clone)[[1L]])
})
