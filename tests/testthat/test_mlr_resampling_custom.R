test_that("custom has duplicated ids", {
  r = rsmp("custom")
  expect_identical(r$duplicated_ids, TRUE)
})

test_that("custom_cv accepts external factor", {
  task = tsk("penguins")
  task$filter(1:10)
  r = rsmp("custom_cv")
  split_f = factor(c(rep(letters[1:3], each = 3), NA))
  r$instantiate(task, split = split_f)

  expect_class(r$instance, "list")
  expect_length(r$instance, 3)
  expect_length(r$train_set(3), 6)

  expect_identical(r$duplicated_ids, FALSE)

})

test_that("custom_cv accepts task feature", {
  task = tsk("german_credit")
  r = rsmp("custom_cv")
  r$instantiate(task, split = "job")

  expect_class(r$instance, "list")
  expect_length(r$instance, 4)
  expect_length(r$train_set(3), 370)

  expect_identical(r$duplicated_ids, FALSE)

})
