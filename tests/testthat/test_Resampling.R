context("Resampling")

test_that("Resampling construction", {
  ids = mlr_resamplings$ids()
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


test_that("par_vals", {
  r = mlr_resamplings$get("bootstrap")
  task = mlr_tasks$get("iris")

  r$par_vals = insert(r$par_vals, list(repeats = 100L))
  expect_identical(r$par_vals$ratio, 1)
  expect_identical(r$par_vals$repeats, 100L)

  r$instantiate(task)
  expect_true(r$is_instantiated)
  expect_identical(r$iters, 100L)
  expect_vector(r$train_set(100), len = task$nrow)

  expect_resampling(r)

  expect_error({
    r$par_vals = list(repeats = 10L)
  }, "equal to set")

  expect_error({
    r$par_vals = list(ratio = 0.5, repeats = 10L, foobar = 12)
  }, "equal to set")
})
