context("predict")


test_that("predict on newdata works / classif", {
  task = tsk("iris")$filter(1:120)
  learner = lrn("classif.featureless")
  expect_error(learner$predict(task), "trained")
  learner$train(task)
  expect_task(learner$state$train_task)
  newdata = tsk("iris")$filter(121:150)$data()

  # passing the task
  p = learner$predict_newdata(newdata = newdata, task = task)
  expect_data_table(as.data.table(p), nrows = 30)
  expect_set_equal(as.data.table(p)$row_id, 1:30)
  expect_factor(p$truth, any.missing = FALSE, levels = task$class_names)

  # rely on internally stored task representation
  p = learner$predict_newdata(newdata = newdata, task = NULL)
  expect_data_table(as.data.table(p), nrows = 30)
  expect_set_equal(as.data.table(p)$row_id, 1:30)
  expect_factor(p$truth, any.missing = FALSE, levels = task$class_names)

  # with missing target column
  newdata$Species = NULL
  p = learner$predict_newdata(newdata = newdata, task = task)
  expect_data_table(as.data.table(p), nrows = 30)
  expect_set_equal(as.data.table(p)$row_id, 1:30)
  expect_factor(p$truth, levels = task$class_names)
  expect_true(allMissing(p$truth))
})


test_that("predict on newdata works / regr", {
  task = tsk("boston_housing")
  train = which(seq_len(task$nrow) %% 2 == 0L)
  test = setdiff(seq_len(task$nrow), train)

  learner = lrn("regr.featureless")
  learner$train(task)

  newdata = task$clone()$filter(test)$data()
  p = learner$predict_newdata(newdata)

  expect_data_table(as.data.table(p), nrows = length(test))
  expect_set_equal(as.data.table(p)$row_id, seq_along(test))
})


test_that("predict on newdata works / no target column", {
  task = tsk("boston_housing")
  train = which(seq_len(task$nrow) %% 2 == 0L)
  test = setdiff(seq_len(task$nrow), train)

  learner = lrn("regr.featureless")
  learner$train(task$clone()$filter(train))

  newdata = remove_named(task$clone()$filter(test)$data(), task$target_names)
  p = learner$predict_newdata(newdata = newdata)

  expect_data_table(as.data.table(p), nrows = length(test))
  expect_set_equal(as.data.table(p)$row_id, seq_along(test))
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

  train$Embarked[train$Embarked == ""] = NA
  train$Embarked = droplevels(train$Embarked)
  test$Embarked[test$Embarked == ""] = NA
  test$Embarked = droplevels(test$Embarked)

  task = TaskClassif$new(id = "titanic", train, target = "Survived", positive = "1")

  lrn = lrn("classif.rpart")

  lrn$train(task)
  p = lrn$predict_newdata(newdata = test)
  expect_prediction_classif(p)
  expect_factor(p$response, levels = task$class_names, any.missing = FALSE)
  expect_factor(p$truth, levels = task$class_names)
  expect_true(allMissing(p$truth))
})

test_that("predict train + test set", {
  task = tsk("iris")
  m1 = msr("debug", id = "tr", predict_sets = "train")
  m2 = msr("debug", id = "te", predict_sets = "test")
  m3 = msr("debug", id = "trte", predict_sets = c("train", "test"))
  measures = list(m1, m2, m3)
  hout = rsmp("holdout")$instantiate(task)
  n_train = length(hout$train_set(1))
  n_test = length(hout$test_set(1))

  learner = lrn("classif.rpart")
  rr = resample(task, learner, hout)
  expect_error(rr$aggregate(measures = measures), "predict.set")

  learner = lrn("classif.rpart", predict_sets = "train")
  rr = resample(task, learner, hout)
  expect_error(rr$aggregate(measures = measures), "predict.set")

  learner = lrn("classif.rpart", predict_sets = c("train", "test"))
  rr = resample(task, learner, hout)
  aggr = rr$aggregate(measures = measures)
  expect_equal(unname(is.na(aggr)), c(FALSE, FALSE, FALSE))
  expect_equal(unname(aggr), c(n_train, n_test, n_train + n_test))
})

test_that("assertions work (#357)", {
  measures = lapply(c("classif.auc","classif.acc"), msr)
  task = tsk("iris")
  lrn = lrn("classif.featureless")
  p = lrn$train(task)$predict(task)

  expect_error(p$score(msr("classif.auc"), "predict.type"))
  expect_error(p$score(msr("regr.mse"), "task.type"))
})
