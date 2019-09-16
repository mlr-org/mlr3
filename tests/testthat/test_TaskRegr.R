context("TaskRegr")

test_that("Basic ops on BostonHousing task", {
  task = tsk("boston_housing")
  expect_task(task)
  expect_task_supervised(task)
  expect_task_regr(task)
  expect_equal(task$target_names, "medv")

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
