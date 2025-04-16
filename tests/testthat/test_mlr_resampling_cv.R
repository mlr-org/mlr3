test_that("cv has no duplicated ids", {
  r = rsmp("cv")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("split into evenly sized groups", {
  task = tsk("iris")
  r = rsmp("cv", folds = 3)
  r$instantiate(task)
  expect_equal(uniqueN(lengths(lapply(1:3, r$train_set))), 1L)
})

test_that("stratification", {
  data = data.table(y = factor(rep(letters[1:2], times = c(90, 10))), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")
  task$col_roles$stratum = task$target_names

  r = rsmp("cv", folds = 5L)
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 72)
    expect_equal(task$data(r$train_set(i))[y == "b", .N], 8)
    expect_equal(task$data(r$test_set(i))[y == "a", .N], 18)
    expect_equal(task$data(r$test_set(i))[y == "b", .N], 2)
  }
})

test_that("grouping", {
  r = rsmp("cv", folds = 3)
  expect_grouping_works(r)
})
