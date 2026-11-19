test_that("subsampling has no duplicated ids", {
  r = rsmp("subsampling")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("stratification", {
  data = data.table(
    y = factor(rep(letters[1:2], times = c(90, 10))),
    x1 = runif(100),
    x2 = rep(LETTERS[1:2], times = c(50, 50))
  )
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")
  task$col_roles$stratum = task$target_names

  r = rsmp("subsampling", ratio = 0.5, repeats = 3)
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 45)
    expect_equal(task$data(r$train_set(i))[y == "b", .N], 5)
    expect_equal(task$data(r$test_set(i))[y == "a", .N], 45)
    expect_equal(task$data(r$test_set(i))[y == "b", .N], 5)
  }
})

test_that("empty training set", {
  task = tsk("iris")$filter(1:4)
  r = rsmp("subsampling", ratio = 0.1, repeats = 2L)$instantiate(task)
  expect_identical(r$train_set(1L), integer())
  expect_set_equal(r$test_set(1L), task$row_ids)

  task = tsk("iris")
  r = rsmp("subsampling", ratio = 0, repeats = 2L)$instantiate(task)
  expect_identical(r$train_set(2L), integer())
  expect_set_equal(r$test_set(2L), task$row_ids)

  r = rsmp("subsampling", ratio = 1, repeats = 2L)$instantiate(task)
  expect_set_equal(r$train_set(2L), task$row_ids)
  expect_identical(r$test_set(2L), integer())
})

test_that("grouping", {
  r = rsmp("subsampling", ratio = 0.5, repeats = 3)
  expect_grouping_works(r)
})
