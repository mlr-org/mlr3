test_that("on_evaluation_begin works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_begin = function(callback, context) {
      expect_task(context$task)
      expect_learner(context$learner)
      expect_resampling(context$resampling)
      expect_null(context$param_values)
      expect_null(context$sets)
      expect_null(context$test_set)
      expect_null(context$predict_sets)
      expect_null(context$pdatas)
    }
  )

  resample(task, learner, resampling, callbacks = callback)

})

test_that("on_evaluation_before_train works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_before_train = function(callback, context) {
      expect_task(context$task)
      expect_learner(context$learner)
      expect_resampling(context$resampling)
      expect_null(context$param_values)
      expect_list(context$sets, len = 2)
      expect_equal(names(context$sets), c("train", "test"))
      expect_integer(context$sets$train)
      expect_integer(context$sets$test)
      expect_null(context$test_set)
      expect_null(context$predict_sets)
      expect_null(context$pdatas)
    }
  )

  resample(task, learner, resampling, callbacks = callback)

})

test_that("on_evaluation_before_predict works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_before_predict = function(callback, context) {
      expect_task(context$task)
      expect_learner(context$learner)
      expect_resampling(context$resampling)
      expect_null(context$param_values)
      expect_list(context$sets, len = 2)
      expect_equal(names(context$sets), c("train", "test"))
      expect_integer(context$sets$train)
      expect_integer(context$sets$test)
      expect_class(context$learner$model, "rpart")
      expect_null(context$test_set)
      expect_equal(context$predict_sets, "test")
      expect_null(context$pdatas)
    }
  )

  resample(task, learner, resampling, callbacks = callback)
})

test_that("on_evaluation_end works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_end = function(callback, context) {
      expect_task(context$task)
      expect_learner(context$learner)
      expect_resampling(context$resampling)
      expect_null(context$param_values)
      expect_list(context$sets, len = 2)
      expect_equal(names(context$sets), c("train", "test"))
      expect_integer(context$sets$train)
      expect_integer(context$sets$test)
      expect_class(context$learner$model, "rpart")
      expect_null(context$test_set)
      expect_equal(context$predict_sets, "test")
      expect_class(context$pdatas$test, "PredictionData")
    }
  )

  resample(task, learner, resampling, callbacks = callback)
})

test_that("writing to learner$state works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_end = function(callback, context) {
      context$learner$state$test = 1
    }
  )

  rr = resample(task, learner, resampling, callbacks = callback)
  expect_equal(rr$learners[[1]]$state$test, 1)
})
