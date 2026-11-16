test_that("partition two way split", {
  task = tsk("sonar")
  li = partition(task, ratio = 0.66)
  expect_list(li, len = 3L)
  expect_names(names(li), identical.to = c("train", "test", "validation"))
  expect_equal(length(li$train), 137)
  expect_equal(length(li$test), 71)
  expect_equal(length(li$validation), 0)

  expect_disjunct(li$train, li$test)
  expect_disjunct(li$train, li$validation)
})

test_that("partition checks ratio", {
  task = tsk("sonar")
  expect_error(partition(task, ratio = NA_real_), "missing")
  expect_error(partition(task, ratio = -0.5), ">= 0")
  expect_error(partition(task, ratio = c(0.5, -0.2)), ">= 0")
  expect_error(partition(task, ratio = c(0.5, 0.5)), "smaller than 1")
  expect_length(partition(task, ratio = 0)$train, 0L)
})

test_that("partition three way split", {
  task = tsk("sonar")
  li = partition(task, ratio = c(0.66, 0.14))
  expect_list(li, len = 3L)
  expect_names(names(li), identical.to = c("train", "test", "validation"))
  expect_equal(length(li$train), 137)
  expect_equal(length(li$test), 29)
  expect_equal(length(li$validation), 42)

  expect_disjunct(li$train, li$test)
  expect_disjunct(li$train, li$validation)
})
