test_that("construction", {
  l = Learner$new("test-learner", task_type = "classif", predict_types = "prob")
  expect_class(l, "Learner")
})

test_that("clone", {
  l1 = lrn("classif.rpart")$train(tsk("iris"))
  l2 = l1$clone(deep = TRUE)

  expect_different_address(l1$state$log, l2$state$log)
  expect_different_address(l1$param_set, l2$param_set)

  l1$param_set$values = list(xval = 10L)
  expect_equal(l1$param_set$values$xval, 10L)
  expect_equal(l2$param_set$values$xval, 0L)
})

test_that("Learners are called with invoke / small footprint of call", {
  task = tsk("boston_housing")
  learner = lrn("regr.rpart")
  learner$train(task)
  call = as.character(learner$model$call)
  expect_character(call, min.len = 1L, any.missing = FALSE)
  expect_true(any(grepl("task$formula()", call, fixed = TRUE)))
  expect_true(any(grepl("task$data", call, fixed = TRUE)))
  expect_lt(sum(nchar(call)), 1000)
})

test_that("Extra data slots of learners are kept / reset", {
  task = tsk("boston_housing")
  learner = lrn("regr.rpart")
  learner$train(task)
  learner$state$foo = "bar"
  expect_equal(learner$state$foo, "bar")
  learner$predict(task)
  expect_equal(learner$state$foo, "bar")

  learner$train(task)
  expect_null(learner$state$foo)
})

test_that("task is checked in train() / predict()", {
  learner = lrn("regr.rpart")
  expect_error(learner$train(tsk("pima")), "type")
  expect_error(learner$predict(tsk("pima")), "type")
})

test_that("learner timings", {
  learner = lrn("regr.rpart")
  t = learner$timings
  expect_equal(unname(t), as.double(c(NA, NA)))
  expect_equal(names(t), c("train", "predict"))


  learner$train(tsk("mtcars"))
  t = learner$timings
  expect_number(t[["train"]])
  expect_equal(t[["predict"]], NA_real_)

  learner$predict(tsk("mtcars"))
  t = learner$timings
  expect_number(t[["train"]])
  expect_number(t[["predict"]])
})

test_that("predict on newdata works / classif", {
  task = tsk("iris")$filter(1:120)
  learner = lrn("classif.featureless")
  expect_error(learner$predict(task), "trained")
  learner$train(task)
  expect_task(learner$state$train_task, null_backend_ok = TRUE)
  newdata = tsk("iris")$filter(121:150)$data()

  # passing the task
  p = learner$predict_newdata(newdata = newdata, task = task)
  expect_data_table(as.data.table(p), nrows = 30)
  expect_set_equal(as.data.table(p)$row_ids, 1:30)
  expect_factor(p$truth, any.missing = FALSE, levels = task$class_names)

  # rely on internally stored task representation
  p = learner$predict_newdata(newdata = newdata, task = NULL)
  expect_data_table(as.data.table(p), nrows = 30)
  expect_set_equal(as.data.table(p)$row_ids, 1:30)
  expect_factor(p$truth, any.missing = FALSE, levels = task$class_names)

  # with missing target column
  newdata$Species = NULL
  p = learner$predict_newdata(newdata = newdata, task = task)
  expect_data_table(as.data.table(p), nrows = 30)
  expect_set_equal(as.data.table(p)$row_ids, 1:30)
  expect_factor(p$truth, levels = task$class_names)
  expect_true(allMissing(p$truth))
})

test_that("train task is properly cloned (#383)", {
  lr = lrn("classif.rpart")$train(tsk("iris"))
  p = lr$predict_newdata(iris[1:3, 1:4])
  expect_equal(length(p$row_ids), 3)
  p = lr$predict_newdata(iris[1:3, 1:4])
  expect_equal(length(p$row_ids), 3)
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
  expect_set_equal(as.data.table(p)$row_ids, seq_along(test))
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
  expect_set_equal(as.data.table(p)$row_ids, seq_along(test))

  # newdata with 1 col and learner that does not support missings
  xdt = data.table(x = 1, y = 1)
  task = as_task_regr(xdt, target = "y")
  learner = lrn("regr.featureless")
  learner$properties = setdiff(learner$properties, "missings")
  learner$train(task)
  learner$predict_newdata(xdt[, 1])
})

test_that("predict on newdata works / empty data frame", {
  task = tsk("mtcars")
  splits = partition(task)
  learner = lrn("regr.featureless")
  learner$train(task$clone()$filter(splits$train))

  newdata = as_data_backend(task$data(rows = integer()))
  p = learner$predict_newdata(newdata = newdata)

  expect_data_table(as.data.table(p), nrows = 0)
  expect_set_equal(as.data.table(p)$row_ids, integer())
})

test_that("predict on newdata works / data backend input", {
  task = tsk("mtcars")
  splits = partition(task)
  learner = lrn("regr.featureless")
  learner$train(task$clone()$filter(splits$train))

  newdata = task$data(rows = splits$test)
  newdata$new_row_id = sample(1e4, nrow(newdata))
  backend = as_data_backend(newdata, primary_key = "new_row_id")
  p = learner$predict_newdata(newdata = backend)

  expect_data_table(as.data.table(p), nrows = backend$nrow)
  expect_set_equal(as.data.table(p)$row_ids, backend$rownames)
})


test_that("predict on newdata works / titanic use case", {
  skip_if_not_installed("mlr3data")
  data("titanic", package = "mlr3data")
  drop = c("cabin", "name", "ticket", "passenger_id")
  data = setDT(remove_named(titanic, drop))
  task = TaskClassif$new(id = "titanic", data[!is.na(survived), ], target = "survived")

  learner = lrn("classif.rpart")$train(task)
  p = learner$predict_newdata(newdata = data[is.na(survived)])
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
  measures = lapply(c("classif.auc", "classif.acc"), msr)
  task = tsk("iris")
  lrn = lrn("classif.featureless")
  p = lrn$train(task)$predict(task)

  expect_error(p$score(msr("classif.auc"), "predict.type"))
  expect_error(p$score(msr("regr.mse"), "task.type"))
})

test_that("reset()", {
  task = tsk("iris")
  lrn = lrn("classif.rpart")

  lrn$train(task)
  expect_list(lrn$state, names = "unique")
  expect_learner(lrn$reset())
  expect_null(lrn$state)
})

test_that("empty predict set (#421)", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("holdout", ratio = 1)
  hout = resampling$instantiate(task)
  learner$train(task, hout$train_set(1))
  pred = learner$predict(task, hout$test_set(1))
  expect_prediction(pred)
  expect_true(any(grepl("No data to predict on", learner$log$msg)))
})

test_that("fallback learner is deep cloned (#511)", {
  l = lrn("classif.rpart")
  l$fallback = lrn("classif.featureless")
  expect_different_address(l$fallback, l$clone(deep = TRUE)$fallback)
})

test_that("learner cannot be trained with TuneToken present", {
  task = tsk("boston_housing")
  learner = lrn("regr.rpart", cp = paradox::to_tune(0.1, 0.3))
  expect_error(learner$train(task),
    regexp = "<LearnerRegrRpart:regr.rpart> cannot be trained with TuneToken present in hyperparameter: cp",
    fixed = TRUE)
})

test_that("integer<->numeric conversion in newdata (#533)", {
  data = data.table(y = runif(10), x = 1:10)
  newdata = data.table(y = runif(10), x = 1:10 + 0.1)

  task = TaskRegr$new("test", data, "y")
  learner = lrn("regr.featureless")
  learner$train(task)
  expect_prediction(learner$predict_newdata(data))
  expect_prediction(learner$predict_newdata(newdata))
})

test_that("weights", {
  data = cbind(iris, w = rep(c(1, 100, 1), each = 50))
  task = TaskClassif$new("weighted_task", data, "Species")
  task$set_col_roles("w", "weight")

  learner = lrn("classif.rpart")
  learner$train(task)

  conf = learner$predict(task)$confusion
  expect_equal(unname(conf[, 2]), c(0, 50, 0)) # no errors in class with weight 100

  expect_prediction(learner$predict_newdata(data[1:3, ]))
  expect_prediction(learner$predict_newdata(iris[1:3, ]))
})

test_that("mandatory properties",  {
  task = tsk("iris")
  learner = lrn("classif.rpart")
  learner$properties = setdiff(learner$properties, "multiclass")
  resample = rsmp("holdout")

  expect_error(learner$train(task), "multiclass")
  expect_error(resample(task, learner, resample), "multiclass")
  expect_error(benchmark(benchmark_grid(task, learner, resample)), "multiclass")
})

test_that("train task is cloned (#382)", {
  train_task = tsk("iris")
  lr = lrn("classif.rpart")$train(train_task)
  expect_different_address(lr$state$train_task, train_task)

  lrc = lr$clone(deep = TRUE)
  expect_different_address(lr$state$train_task, lrc$state$train_task)
})

test_that("Error on missing data (#413)", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  learner$properties = setdiff(learner$properties, "missings")
  expect_error(learner$train(task), "missing values")
})
