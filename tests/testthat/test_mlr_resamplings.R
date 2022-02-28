test_that("mlr_resamplings", {
  expect_dictionary(mlr_resamplings, min_items = 1L)
  keys = setdiff(mlr_resamplings$keys(), "ordered_holdout")

  task = tsk("iris")
  for (key in keys) {
    r = rsmp(key)
    expect_resampling(r) # construction works
    expect_false(r$is_instantiated)
    if (key == "custom") {
      ret = r$instantiate(task, list(1:3), list(5:9))
    } else if (key == "custom_cv") {
      ret = r$instantiate(task, iris$Species)
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

test_that("extra_cols", {
  tab = as.data.table(mlr_resamplings, extract = function(x) list(is_instantiated = x$is_instantiated))
  expect_data_table(tab)
  expect_logical(tab$is_instantiated)
  expect_false(any(tab$is_instantiated))
})
