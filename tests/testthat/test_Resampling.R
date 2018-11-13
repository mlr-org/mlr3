context("Resampling")

test_that("Resampling construction", {
  ids = mlr_resamplings$keys()
  task = mlr_tasks$get("iris")
  for (key in ids) {
    r = mlr_resamplings$get(key)
    expect_resampling(r) # construction works
    expect_false(r$is_instantiated)
    if (key == "custom") {
      ret = r$instantiate(task, list(1:3), list(5:9))
    } else {
      ret = r$instantiate(task)
    }
    expect_r6(ret, "Resampling")
    expect_true(r$is_instantiated)
    expect_resampling(r)
  }
})


test_that("param_vals", {
  r = mlr_resamplings$get("bootstrap")
  task = mlr_tasks$get("iris")

  r$param_vals = insert(r$param_vals, list(repeats = 100L))
  expect_identical(r$param_vals$ratio, 1)
  expect_identical(r$param_vals$repeats, 100L)

  r$instantiate(task)
  expect_true(r$is_instantiated)
  expect_identical(r$iters, 100L)
  expect_vector(r$train_set(100), len = task$nrow)

  expect_resampling(r)

  expect_error({
    r$param_vals = list(repeats = 10L)
  }, "equal to set")

  expect_error({
    r$param_vals = list(ratio = 0.5, repeats = 10L, foobar = 12)
  }, "equal to set")
})

test_that("hashing", {
  task = mlr_tasks$get("iris")
  ids = setdiff(mlr_resamplings$keys(), "custom")

  for (id in ids) {
    r = mlr_resamplings$get(id)

    withr::with_seed(123L, r$instantiate(task))
    expect_identical(private(r)$.hash, NA_character_)
    chk = r$hash
    expect_string(chk, pattern = "^[a-z0-9]+$")
    expect_identical(r$hash, chk)
    expect_identical(private(r)$.hash, chk)

    withr::with_seed(123L, r$instantiate(task))
    expect_identical(private(r)$.hash, NA_character_)
    expect_identical(r$hash, chk)

    withr::with_seed(124L, r$instantiate(task))
    expect_false(r$hash == chk)
  }
})

test_that("has_duplicates", {
  task = mlr_tasks$get("iris")

  r = mlr_resamplings$get("custom")
  expect_identical(r$has_duplicates, NA)

  r = mlr_resamplings$get("bootstrap")
  expect_identical(r$has_duplicates, TRUE)

  r = mlr_resamplings$get("cv")
  expect_identical(r$has_duplicates, FALSE)
})

test_that("stratification", {
  data = data.table(y = rep(letters[1:2], times = c(90, 10)), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")

  # bootstrap
  r = mlr_resamplings$get("bootstrap")
  r$param_vals = list(ratio = 1, repeats = 3)
  r$stratify = "y"
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 90)
    expect_equal(task$data(r$train_set(i))[y == "b", .N], 10)
  }

  # holdout
  r = mlr_resamplings$get("holdout")
  r$param_vals = list(ratio = 0.5)
  r$stratify = "y"
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 45)
    expect_equal(task$data(r$train_set(i))[y == "b", .N],  5)
    expect_equal(task$data(r$test_set(i))[y == "a", .N],  45)
    expect_equal(task$data(r$test_set(i))[y == "b", .N],   5)
  }

  # subsampling
  r = mlr_resamplings$get("subsampling")
  r$param_vals = list(ratio = 0.5, repeats = 3)
  r$stratify = "y"
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 45)
    expect_equal(task$data(r$train_set(i))[y == "b", .N],  5)
    expect_equal(task$data(r$test_set(i))[y == "a", .N],  45)
    expect_equal(task$data(r$test_set(i))[y == "b", .N],   5)
  }

  # CV
  r = mlr_resamplings$get("cv")
  r$param_vals = list(folds = 5)
  r$stratify = "y"
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 72)
    expect_equal(task$data(r$train_set(i))[y == "b", .N],  8)
    expect_equal(task$data(r$test_set(i))[y == "a", .N],  18)
    expect_equal(task$data(r$test_set(i))[y == "b", .N],   2)
  }

  # RCV
  r = mlr_resamplings$get("repeated_cv")
  r$param_vals = list(folds = 5, repeats = 2)
  r$stratify = "y"
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 72)
    expect_equal(task$data(r$train_set(i))[y == "b", .N],  8)
    expect_equal(task$data(r$test_set(i))[y == "a", .N],  18)
    expect_equal(task$data(r$test_set(i))[y == "b", .N],   2)
  }

  # error for min group size
  r = mlr_resamplings$get("cv")
  r$param_vals = list(folds = 11)
  r$stratify = "y"
  expect_error(r$instantiate(task), "combination")
})
