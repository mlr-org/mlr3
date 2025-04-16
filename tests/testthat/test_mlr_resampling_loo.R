test_that("loo has no duplicated ids", {
  r = rsmp("loo")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("stratification", {
  data = data.table(y = factor(rep(letters[1:2], times = c(90, 10))), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")
  task$col_roles$stratum = task$target_names

  r = rsmp("loo")
  r$instantiate(task)

  i = 1L
  expect_data_table(task$data(r$train_set(i)), nrows = 99)
  expect_data_table(task$data(r$test_set(i)), nrows = 1)
})

test_that("grouping", {
  r = rsmp("loo")
  expect_grouping_works(r)
})
