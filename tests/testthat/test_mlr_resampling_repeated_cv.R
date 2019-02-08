context("mlr_resampling_repeated_cv")

test_that("repeated cv has no duplicated ids", {
  r = mlr_resamplings$get("repeated_cv")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("stratification", {
  data = data.table(y = rep(letters[1:2], times = c(90, 10)), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")

  r = mlr_resamplings$get("repeated_cv")
  r$param_set$values = list(folds = 5, repeats = 2)
  r$stratify = "y"
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 72)
    expect_equal(task$data(r$train_set(i))[y == "b", .N],  8)
    expect_equal(task$data(r$test_set(i))[y == "a", .N],  18)
    expect_equal(task$data(r$test_set(i))[y == "b", .N],   2)
  }
})

test_that("grouping", {
  r = mlr_resamplings$get("repeated_cv")
  r$param_set$values = list(folds = 5, repeats = 2)
  expect_grouping_works(r)
})
