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

  newdata = remove_named(task$clone()$filter(test)$data(), task$target_names)
  e$predict(newdata = newdata)

  expect_data_table(as.data.table(e$prediction), nrow = length(test))
  expect_set_equal(as.data.table(e$prediction)$row_id, 507:759)
  expect_set_equal(e$task$row_ids, c(train, 507:759))
})


test_that("predict on newdata works / titanic use case", {
  skip_if_not_installed("titanic")
  train = load_dataset("titanic_train", package = "titanic")
  test = load_dataset("titanic_test", package = "titanic")
  drop = c("Cabin", "Name", "Ticket", "PassengerId")

  train = remove_named(train, drop)
  test = remove_named(test, drop)

  train$Embarked = factor(train$Embarked)
  test$Embarked = factor(test$Embarked, levels = levels(train$Embarked))
  train$Sex = factor(train$Sex)
  test$Sex = factor(test$Sex, levels = levels(train$Sex))

  median_age = median(train$Age, na.rm = TRUE)
  train$Age[is.na(train$Age)] = median_age
  test$Age[is.na(test$Age)] = median_age

  train$Survived = factor(train$Survived)

  task = TaskClassif$new(id = "titanic", train, target = "Survived", positive = "1")
  lrn = mlr_learners$get("classif.rpart")

  e = Experiment$new(task, lrn)$train()
  e$predict(newdata = test)
  expect_experiment(e)
  p = e$prediction
  expect_prediction_classif(p)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_factor(p$truth, levels = task$class_names)
  expect_true(allMissing(p$truth))
})
