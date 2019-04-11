context("mlr_resampling_subsampling")

test_that("subsampling has no duplicated ids", {
  r = mlr_resamplings$get("subsampling")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("stratification", {
  data = data.table(y = rep(letters[1:2], times = c(90, 10)), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")

  r = mlr_resamplings$get("subsampling")
  r$param_set$values = list(ratio = 0.5, repeats = 3, stratify = TRUE)
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 45)
    expect_equal(task$data(r$train_set(i))[y == "b", .N],  5)
    expect_equal(task$data(r$test_set(i))[y == "a", .N],  45)
    expect_equal(task$data(r$test_set(i))[y == "b", .N],   5)
  }
})

test_that("grouping", {
  r = mlr_resamplings$get("subsampling")
  r$param_set$values = list(ratio = 0.5, repeats = 3)
  expect_grouping_works(r)
})
