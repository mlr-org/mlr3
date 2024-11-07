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
  task = tsk("california_housing")
  learner = lrn("regr.rpart")
  learner$train(task)
  call = as.character(learner$model$call)
  expect_character(call, min.len = 1L, any.missing = FALSE)
  expect_true(any(grepl("task$formula()", call, fixed = TRUE)))
  expect_true(any(grepl("task$data", call, fixed = TRUE)))
  expect_lt(sum(nchar(call)), 1000)
})

test_that("Extra data slots of learners are kept / reset", {
  task = tsk("california_housing")
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
  task = tsk("california_housing")
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
  task = tsk("california_housing")
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
  m1 = msr("debug_classif", id = "tr", predict_sets = "train")
  m2 = msr("debug_classif", id = "te", predict_sets = "test")
  m3 = msr("debug_classif", id = "trte", predict_sets = c("train", "test"))
  measures = list(m1, m2, m3)
  hout = rsmp("holdout")$instantiate(task)
  n_train = length(hout$train_set(1))
  n_test = length(hout$test_set(1))

  learner = lrn("classif.rpart")
  rr = resample(task, learner, hout)
  expect_warning(expect_warning(rr$aggregate(measures = measures), "predict sets", fixed = TRUE))

  learner = lrn("classif.rpart", predict_sets = "train")
  rr = resample(task, learner, hout)
  expect_warning(expect_warning(rr$aggregate(measures = measures), "predict sets", fixed = TRUE))

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
  l$encapsulate("evaluate",lrn("classif.featureless"))
  expect_different_address(l$fallback, l$clone(deep = TRUE)$fallback)
})

test_that("learner cannot be trained with TuneToken present", {
  task = tsk("california_housing")
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

test_that("Task prototype is stored in state", {
  task = tsk("california_housing")
  learner = lrn("regr.rpart")
  learner$train(task)

  prototype = learner$state$data_prototype
  expect_data_table(prototype, nrows = 0, ncols = 10)
  expect_names(names(prototype), permutation.of = c(task$feature_names, task$target_names))
})

test_that("Models can be replaced", {
  task = tsk("california_housing")
  learner = lrn("regr.featureless")
  learner$train(task)

  learner$model$location = 1
  expect_equal(learner$model$location, 1)
})

test_that("validation task's backend is removed", {
  learner = lrn("regr.rpart")
  task = tsk("mtcars")
  task$internal_valid_task = 1:10
  learner$train(task)
  expect_true(is.null(learner$state$train_task$internal_valid_task$backend))
})

test_that("manual $train() stores validation hash and validation ids", {
  task = tsk("iris")
  l = lrn("classif.debug", validate = 0.2)
  l$train(task)
  expect_character(l$state$internal_valid_task_hash)

  l = lrn("classif.debug", validate = "predefined")
  task = tsk("iris")
  task$internal_valid_task = 1:10
  l$train(task)
  expect_equal(l$state$internal_valid_task_hash, task$internal_valid_task$hash)

  # nothing is stored for learners that don't do it
  l2 = lrn("classif.featureless")
  l2$train(task)
  expect_true(is.null(l2$state$internal_valid_task_hash))
})

test_that("error when training a learner that sets valiadte to 'predefined' on a task without a validation task", {
  task = tsk("iris")
  learner = lrn("classif.debug", validate = "predefined")
  expect_error(learner$train(task), "is set to 'predefined'")
  task$internal_valid_task = 1:10
  expect_class(learner, "Learner")
})

test_that("properties are also checked on validation task", {
  task = tsk("iris")
  row = task$data(1)
  row[[1]][1] = NA
  row$..row_id = 151
  task$rbind(row)
  task$internal_valid_task = 151
  learner = lrn("classif.debug", validate = "predefined")
  learner$properties = setdiff(learner$properties, "missings")
  expect_error(learner$train(task), "missing values")
})

test_that("validate changes phash and hash", {
  l1 = lrn("classif.debug")
  l2 = lrn("classif.debug")
  l2$validate = 0.2
  expect_false(l1$hash == l2$hash)
})

test_that("marshaling and encapsulation", {
  task = tsk("iris")
  learner = lrn("classif.debug", count_marshaling = TRUE)

  # callr encapsulation causes marshaling
  learner$encapsulate("callr", lrn("classif.featureless"))
  learner$train(task)
  expect_equal(learner$model$marshal_count, 1)
  expect_false(learner$marshaled)
  expect_prediction(learner$predict(task))

  # no marshaling with no other encapsulation
  learner$encapsulate("none")
  learner$train(task)
  expect_equal(learner$model$marshal_count, 0)
})

test_that("marshal state", {
  state = lrn("classif.debug")$train(tsk("iris"))$state
  sm = marshal_model(state)
  expect_true(is_marshaled_model(sm))
  expect_equal(state, unmarshal_model(marshal_model(state)))
})


test_that("internal_valid_task is created correctly", {
  LearnerClassifTest = R6Class("LearnerClassifTest", inherit = LearnerClassifDebug,
    public = list(
      task = NULL
    ),
    private = list(
      .train = function(task, ...)  {
        self$task = task$clone(deep = TRUE)
        super$.train(task, ...)
      }
    )
  )
  # validate = NULL (but task has one)
  learner = LearnerClassifTest$new()
  task = tsk("iris")
  task$internal_valid_task = partition(task)$test
  learner$train(task)
  learner$validate = NULL
  expect_true(is.null(learner$internal_valid_scores))
  expect_true(is.null(learner$task$internal_valid_task))

  # validate = NULL (but task has none)
  learner1 = LearnerClassifTest$new()
  task1 = tsk("iris")
  learner1$train(task1)
  expect_true(is.null(learner1$internal_valid_scores))
  expect_true(is.null(learner1$task$internal_valid_task))

  # validate = "test"
  LearnerClassifTest2 = R6Class("LearnerClassifTest2", inherit = LearnerClassifDebug,
    public = list(
      expected_valid_ids = NULL,
      expected_train_ids = NULL
    ),
    private = list(
      .train = function(task, ...)  {
        if (!test_permutation(task$internal_valid_task$row_ids, self$expected_valid_ids)) {
          stopf("something went wrong")
        }
        if (!test_permutation(task$row_ids, self$expected_train_ids)) {
          stopf("something went wrong")
        }
        super$.train(task, ...)
      }
    )
  )
  task2 = tsk("iris")
  learner2 = LearnerClassifTest2$new()
  learner2$validate = "test"
  resampling = rsmp("holdout")$instantiate(task2)
  learner2$expected_valid_ids = resampling$test_set(1)
  learner2$expected_train_ids = resampling$train_set(1)
  expect_error(resample(task2, learner2, resampling), regexp = NA)

  # ratio works
  LearnerClassifTest3 = R6Class("LearnerClassifTest3", inherit = LearnerClassifDebug,
    private = list(
      .train = function(task, ...)  {
        if (length(task$internal_valid_task$row_ids) != 20) {
          stopf("something went wrong")
        }
        super$.train(task, ...)
      }
    )
  )
  task = tsk("iris")$filter(1:100)
  learner3 = LearnerClassifTest3$new()
  learner3$validate = 0.2
  learner3$train(task)

  # check that validation task is reset discarded at the end
  learner4 = lrn("classif.debug", validate = 0.2)
  task = tsk("iris")
  learner4$train(task)
  expect_true(is.null(task$internal_valid_task))
})

test_that("compatability check on validation task", {
  learner = lrn("classif.debug", validate = "predefined")
  task = tsk("german_credit")
  task$internal_valid_task = 1:10
  task$col_roles$feature = "age"
  expect_error(learner$train(task), "has different features")
  task$internal_valid_task$col_roles$feature = "age"
  task$internal_valid_task$col_roles$target = "credit_history"
  expect_error(learner$train(task), "has different target")
})

test_that("model is marshaled during parallel predict", {
  # by setting check_pid = TRUE, we ensure that unmarshal_model() sets the process id to the current
  # id. LearnerClassifDebug then checks during `.predict()`, whether the marshal_id of the model is equal to the current process id and errs if this is not the case.
  task = tsk("iris")
  learner = lrn("classif.debug", check_pid = TRUE)
  learner$train(task)
  learner$parallel_predict = TRUE
  pred = with_future(future::multisession, {
    learner$predict(task)
  })
  expect_class(pred, "Prediction")
})

test_that("model is marshaled during callr prediction", {
  # by setting check_pid = TRUE, we ensure that unmarshal_model() sets the process id to the current
  # id. LearnerClassifDebug then checks during `.predict()`, whether the marshal_id of the model is equal to the current process id and errs if this is not the case.
  task = tsk("iris")
  learner = lrn("classif.debug", check_pid = TRUE)
  learner$encapsulate("callr", lrn("classif.featureless"))
  learner$train(task)
  pred = learner$predict(task)
  expect_class(pred, "Prediction")
})

test_that("predict leaves marshaling status as-is", {
  task = tsk("iris")
  learner = lrn("classif.debug", check_pid = TRUE)
  learner$encapsulate("callr", lrn("classif.featureless"))
  learner$train(task)
  learner$marshal()
  expect_class(learner$predict(task), "Prediction")
  expect_true(learner$marshaled)
  learner$unmarshal()
  expect_class(learner$predict(task), "Prediction")
  expect_false(learner$marshaled)
})

test_that("learner state contains validate field", {
  learner = lrn("classif.debug", validate = 0.2)
  learner$train(tsk("iris"))
  expect_equal(learner$state$validate, 0.2)
})

test_that("learner state contains internal valid task information", {
  task = tsk("iris")
  # 1. resample
  learner = lrn("classif.debug", validate = 0.2)
  rr = resample(task, learner, rsmp("holdout"))
  expect_string(rr$learners[[1L]]$state$internal_valid_task_hash)

  # 1. manual
  learner$train(task)
  expect_string(learner$state$internal_valid_task_hash)
})

test_that("validation task with 0 observations", {
  learner = lrn("classif.debug", validate = "predefined")
  task = tsk("iris")
  task$internal_valid_task = integer(0)
  expect_error({learner$train(task)}, "has 0 observations")
})

test_that("column info is compared during predict", {
  x = c("a", "b")
  d = data.table(
    x = factor(x, levels = c("a", "b")),
    y = 1:2
  )

  dflip = data.table(
    x = factor(x, labels = c("b", "a"), levels = c("b", "a")),
    y = 1:2
  )

  dother = data.table(
    x1 = factor(c("a", "b")),
    y = 1:2
  )

  task = as_task_classif(d, target = "y")
  task_flip = as_task_classif(dflip, target = "y")
  task_other = as_task_classif(dother, target = "y")
  l = lrn("classif.rpart")
  l$train(task)
  old_threshold = lg$threshold
  lg$set_threshold("warn")
  expect_output(l$predict(task_flip), "task with different column info")
  lg$set_threshold(old_threshold)
  expect_error(l$predict(task_other), "with different columns")
})

test_that("quantiles in LearnerRegr", {
  task = tsk("mtcars")
  learner = lrn("regr.debug", predict_type = "quantiles")
  expect_learner(learner)
  quantiles = c(0.05, 0.5, 0.95)
  learner$quantiles = quantiles

  expect_numeric(learner$quantiles, any.missing = FALSE, len = 3)

  learner$quantile_response = 0.6
  expect_equal(learner$quantile_response, 0.6)
  expect_equal(learner$quantiles, c(0.05, 0.5, 0.6, 0.95))

  expect_error({
    learner$quantiles = c(0.5, 0.1)
  }, "sorted")

  expect_error({
    learner$quantiles = integer()
  }, "length")

  learner$train(task)

  expect_numeric(learner$model$quantiles, len = 4L)

  pred = learner$predict(task)
  expect_prediction(pred)
  expect_subset("quantiles", pred$predict_types)
  expect_matrix(pred$quantiles, ncols = 4L, nrows = task$nrow, any.missing = FALSE)
  expect_true(!any(apply(pred$quantiles, 1L, is.unsorted)))
  expect_equal(pred$response, pred$quantiles[, 3L])

  tab = as.data.table(pred)
  expect_data_table(tab, nrows = task$nrow)
  expect_subset("q0.5", names(tab))
})

test_that("predict time is cumulative", {
  learner = lrn("classif.debug", sleep_predict = function() 0.05)
  task = tsk("iris")
  learner$train(task)$predict(task)
  t1 = learner$timings["predict"]
  learner$param_set$values$sleep_predict = function() 0.01
  learner$predict(task)
  t2 = learner$timings["predict"]
  expect_true(t1 > t2)
})
