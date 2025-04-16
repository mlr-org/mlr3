test_that("insample has no duplicated ids", {
  r = rsmp("insample")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("stratification", {
  data = data.table(y = factor(rep(letters[1:2], times = c(90, 10))), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")
  task$col_roles$stratum = task$target_names

  r = rsmp("insample")
  r$instantiate(task)

  i = 1L
  expect_set_equal(r$train_set(i), task$row_ids)
  expect_set_equal(r$test_set(i), task$row_ids)
})

test_that("grouping", {
  r = rsmp("insample")
  expect_grouping_works(r)
})
