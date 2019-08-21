context("Resampling")

test_that("param_vals", {
  task = tsk("iris")
  r = rsmp("bootstrap", repeats = 100L, ratio = 1)

  expect_identical(r$param_set$values$ratio, 1)
  expect_identical(r$param_set$values$repeats, 100L)

  r$instantiate(task)
  expect_true(r$is_instantiated)
  expect_identical(r$iters, 100L)
  expect_atomic_vector(r$train_set(100), len = task$nrow)

  expect_resampling(r)

  expect_error({
    r$param_set$values = list(repeats = 10L)
  }, "ratio")

  expect_error({
    r$param_set$values = list(ratio = 0.5, repeats = 10L, foobar = 12)
  }, "foobar")
})

test_that("hashing", {
  task = tsk("iris")
  keys = setdiff(mlr_resamplings$keys(), "custom")

  for (key in keys) {
    r = rsmp(key)

    with_seed(123L, r$instantiate(task))
    hash = r$hash
    expect_string(hash, pattern = "^[a-z0-9]+$")

    with_seed(123L, r$instantiate(task))
    expect_identical(r$hash, hash)

    with_seed(124L, r$instantiate(task))
    expect_false(identical(r$hash, hash))
  }
})
