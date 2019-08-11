context("mlr_resamplings")

test_that("mlr_resamplings", {
  expect_dictionary(mlr_resamplings, min_items = 1L)
  keys = mlr_resamplings$keys()

  task = mlr_tasks$get("iris")
  for (key in keys) {
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

test_that("mlr_resamplings: sugar", {
  r = rsmp("cv", id = "cv3", folds = 3L)
  expect_equal(r$id, "cv3")
  expect_equal(r$param_set$values$folds, 3L)
})
