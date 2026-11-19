test_that("custom has duplicated ids", {
  r = rsmp("custom")
  expect_identical(r$duplicated_ids, TRUE)
})

test_that("custom checks the sets", {
  task = tsk("iris")
  r = rsmp("custom")

  expect_error(r$instantiate(task, train_sets = list(1:10), test_sets = NULL), "test_sets")
  expect_error(r$instantiate(task, train_sets = list(), test_sets = list()), "train_sets")
  expect_error(r$instantiate(task, train_sets = list(c(1, 2.5)), test_sets = list(3)), "train_sets")
  expect_error(r$instantiate(task, train_sets = list(1:2), test_sets = list("a")), "test_sets")
  expect_error(r$instantiate(task, train_sets = list(1:2), test_sets = list(151L)), "subset")
  expect_false(r$is_instantiated)

  r$instantiate(task, train_sets = list(c(1, 2), 3), test_sets = list(3, integer()))
  expect_identical(r$train_set(1L), 1:2)
  expect_identical(r$test_set(1L), 3L)
  expect_identical(r$test_set(2L), integer())
  r2 = rsmp("custom")$instantiate(task, train_sets = list(1:2, 3L), test_sets = list(3L, integer()))
  expect_identical(r$hash, r2$hash)
})

test_that("custom_cv accepts a character vector", {
  task = tsk("iris")
  ccv = rsmp("custom_cv")$instantiate(task, f = rep(c("a", "b", NA), 50L))
  expect_identical(ccv$iters, 2L)
  expect_length(ccv$test_set(1L), 50L)
  expect_error(rsmp("custom_cv")$instantiate(task, f = rep(1:3, 50L)), "factor")
  expect_error(rsmp("custom_cv")$instantiate(task, f = c("a", "b")), "length")
})

test_that("custom_cv accepts external factor", {
  task = tsk("penguins")
  task$filter(1:10)

  ccv = rsmp("custom_cv")
  split_f = factor(c(rep(letters[1:3], each = 3), NA))
  ccv$instantiate(task, f = split_f)

  expect_class(ccv$instance, "list")
  expect_length(ccv$instance, 3)
  expect_length(ccv$train_set(3), 6)

  expect_identical(ccv$duplicated_ids, FALSE)
})

test_that("custom_cv accepts task feature", {
  task = tsk("german_credit")
  ccv = rsmp("custom_cv")
  expect_identical(ccv$duplicated_ids, FALSE)

  ccv$instantiate(task, f = task$data(cols = "job")[[1L]])
  expect_class(ccv$instance, "list")
  expect_length(ccv$instance, 4)
  expect_length(ccv$train_set(3), 370)

  ccv$instantiate(task, col = "job")
  expect_class(ccv$instance, "list")
  expect_length(ccv$instance, 4)
  expect_length(ccv$train_set(3), 370)
})
