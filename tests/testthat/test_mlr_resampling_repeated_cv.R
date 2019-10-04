context("mlr_resampling_repeated_cv")

test_that("repeated cv has no duplicated ids", {
  r = rsmp("repeated_cv")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("folds first, then repetitions", {
  task = tsk("iris")
  rrcv = rsmp("repeated_cv", repeats = 2, folds = 3)
  rrcv$instantiate(task)

  expect_integer(intersect(intersect(rrcv$test_set(1), rrcv$test_set(2)), rrcv$test_set(3)), len = 0L)

  expect_equal(rrcv$folds(seq_len(rrcv$iters)), rep(1:2, 3))
  expect_equal(rrcv$repeats(seq_len(rrcv$iters)), rep(1:2, each = 3))
})

test_that("stratification", {
  data = data.table(y = rep(letters[1:2], times = c(90, 10)), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  task = TaskClassif$new("stratify_data", data, target = "y")
  task$col_roles$stratify = task$target_names

  r = rsmp("repeated_cv", folds = 5, repeats = 2)
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 72)
    expect_equal(task$data(r$train_set(i))[y == "b", .N], 8)
    expect_equal(task$data(r$test_set(i))[y == "a", .N], 18)
    expect_equal(task$data(r$test_set(i))[y == "b", .N], 2)
  }
})

test_that("grouping", {
  r = rsmp("repeated_cv", folds = 5, repeats = 2)
  expect_grouping_works(r)
})
