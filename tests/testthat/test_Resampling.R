context("Resampling")

test_that("param_vals", {
  r = mlr_resamplings$get("bootstrap")
  task = mlr_tasks$get("iris")

  r$param_vals = insert_named(r$param_vals, list(repeats = 100L))
  expect_identical(r$param_vals$ratio, 1)
  expect_identical(r$param_vals$repeats, 100L)

  r$instantiate(task)
  expect_true(r$is_instantiated)
  expect_identical(r$iters, 100L)
  expect_vector(r$train_set(100), len = task$nrow)

  expect_resampling(r)

  expect_error({
    r$param_vals = list(repeats = 10L)
  }, "ratio")

  expect_error({
    r$param_vals = list(ratio = 0.5, repeats = 10L, foobar = 12)
  }, "subset")
})

test_that("hashing", {
  task = mlr_tasks$get("iris")
  ids = setdiff(mlr_resamplings$ids(), "custom")

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
