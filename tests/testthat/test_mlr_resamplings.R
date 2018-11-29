context("mlr_resamplings")


test_that("mlr_resamplings", {
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
