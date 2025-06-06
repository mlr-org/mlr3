test_that("Basic ops on Ames Housing task", {
  task = tsk("california_housing")
  expect_task(task)
  expect_task_supervised(task)
  expect_task_regr(task)
  expect_equal(task$target_names, "median_house_value")

  f = task$formula()
  expect_class(f, "formula")
  # expect_set_equal(attr(terms(f), "term.labels"), task$feature_names)
})

test_that("Target is numeric", {
  b = as_data_backend(iris)
  expect_error(TaskRegr$new("iris", backend = b, target = "Species"), "Target column")
})

test_that("TaskRegr: 0 feature task", {
  b = as_data_backend(data.table(y = runif(20)))
  task = TaskRegr$new(id = "zero_feat_task", b, target = "y")
  expect_output(print(task))
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_task_supervised(task)
  expect_task_regr(task)
  expect_data_table(task$data(), ncols = 1L)

  lrn = lrn("regr.featureless")
  p = lrn$train(task)$predict(task)
  expect_prediction(p)
})

test_that("$add_strata", {
  tab = data.table(y = rep(c(1, 10), times = c(10, 10)), x = 1)
  task = TaskRegr$new("strata", tab, "y")
  expect_equal(task$col_roles$stratum, character())

  task$add_strata(task$target_names, bins = 2)
  expect_equal(task$col_roles$stratum, "..stratum_y")
  expect_equal(lengths(task$strata$row_id), c(10L, 10L))

  r = rsmp("holdout", ratio = 0.5)$instantiate(task)
  expect_equal(as.integer(table(r$train_set(1) <= 10L)), c(5L, 5L))
  expect_equal(as.integer(table(r$test_set(1) > 10L)), c(5L, 5L))

  tab = data.table(y = rep(c(1, 10), times = c(50, 10)), x = 1)
  task = TaskRegr$new("strata", tab, "y")
  task$add_strata(task$target_names, bins = 2)
  expect_identical(task$strata$N, c(50L, 10L))
})

test_that("offset column role works", {
  task = tsk("mtcars")
  expect_null(task$offset)
  task$set_col_roles("am", "offset")

  expect_subset("offset", task$properties)
  expect_data_table(task$offset, nrows = task$nrow, ncols = 2)
  expect_subset(c("row_id", "offset"), names(task$offset))

  expect_error({
    task$col_roles$offset = c("am", "gear")
  }, "up to one")

  task$col_roles$offset = character()
  expect_true("offset" %nin% task$properties)
  expect_null(task$offset)
})
