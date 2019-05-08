context("TaskRegr")

test_that("Basic ops on BostonHousing task", {
  task = mlr_tasks$get("boston_housing")
  expect_task(task)
  expect_task_supervised(task)
  expect_task_regr(task)
  expect_equal(task$target_names, "medv")

  f = task$formula()
  expect_class(f, "formula")
  expect_set_equal(attr(terms(f), "term.labels"), task$feature_names)
})

test_that("Target is numeric", {
  b = as_data_backend(iris)
  expect_error(TaskRegr$new("iris", backend = b, target = "Species"), "Target column")
})

test_that("Replace features", {
  task = mlr_tasks$get("boston_housing")
  data = task$data()[, task$feature_names[1:3], with = FALSE]
  task$replace_features(data)
  expect_task(task)
  expect_task_regr(task)
  expect_equal(task$nrow, mlr_tasks$get("boston_housing")$nrow)
  expect_equal(task$ncol, 4L)
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
  expect_data_table(task$data(), ncol = 1L)

  lrn = mlr_learners$get("regr.featureless")
  e = Experiment$new(task, lrn)
  e$train()$predict()$score()
  expect_experiment(e)
  expect_number(e$performance)
})
