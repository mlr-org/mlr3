test_that("custom has duplicated ids", {
  r = rsmp("custom")
  expect_subset("duplicated_ids", r$properties)
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

  expect_disjunct("duplicated_ids", ccv$properties)
})

test_that("custom_cv accepts task feature", {
  task = tsk("german_credit")
  ccv = rsmp("custom_cv")
  expect_disjunct("duplicated_ids", ccv$properties)

  ccv$instantiate(task, f = task$data(cols = "job")[[1L]])
  expect_class(ccv$instance, "list")
  expect_length(ccv$instance, 4)
  expect_length(ccv$train_set(3), 370)

  ccv$instantiate(task, col = "job")
  expect_class(ccv$instance, "list")
  expect_length(ccv$instance, 4)
  expect_length(ccv$train_set(3), 370)
})
