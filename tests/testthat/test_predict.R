context("predict")


test_that("predict on newdata works / classif", {
  task = mlr_tasks$get("iris")$filter(1:120)
  e = Experiment$new(task, "classif.featureless")
  e$train()

  newdata = mlr_tasks$get("iris")$filter(121:150)$data()
  e$predict(newdata = newdata)
  expect_data_table(as.data.table(e$prediction), nrow = 30)
  expect_set_equal(as.data.table(e$prediction)$row_id, 121:150)
})


test_that("predict on newdata works / regr", {
  task = mlr_tasks$get("boston_housing")
  train = which(seq_len(task$nrow) %% 2 == 0L)
  test = setdiff(seq_len(task$nrow), train)

  e = Experiment$new(task$clone()$filter(train), "regr.featureless")
  e$train()

  newdata = task$clone()$filter(test)$data()
  e$predict(newdata = newdata)

  expect_data_table(as.data.table(e$prediction), nrow = length(test))
  expect_set_equal(as.data.table(e$prediction)$row_id, 507:759)
  expect_set_equal(e$task$row_ids, c(train, 507:759))
})


test_that("predict on newdata works / no target column", {
  task = mlr_tasks$get("boston_housing")
  train = which(seq_len(task$nrow) %% 2 == 0L)
  test = setdiff(seq_len(task$nrow), train)

  e = Experiment$new(task$clone()$filter(train), "regr.featureless")
  e$train()

  newdata = task$clone()$filter(test)$select(task$feature_names)$data()
  e$predict(newdata = newdata)

  expect_data_table(as.data.table(e$prediction), nrow = length(test))
  expect_set_equal(as.data.table(e$prediction)$row_id, 507:759)
  expect_set_equal(e$task$row_ids, c(train, 507:759))
})
