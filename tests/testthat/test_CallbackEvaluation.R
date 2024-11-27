test_that("on_evaluation_begin works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_begin = function(callback, context) {
      # expect_* does not work
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      assert_null(context$param_values)
      assert_null(context$sets)
      assert_null(context$test_set)
      assert_null(context$predict_sets)
      assert_null(context$pdatas)
    }
  )

  expect_resample_result(resample(task, learner, resampling, callbacks = callback))
})

test_that("on_evaluation_before_train works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_before_train = function(callback, context) {
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      assert_null(context$param_values)
      assert_list(context$sets, len = 2)
      assert_names(names(context$sets), identical.to = c("train", "test"))
      assert_integer(context$sets$train)
      assert_integer(context$sets$test)
      assert_null(context$test_set)
      assert_null(context$predict_sets)
      assert_null(context$pdatas)
    }
  )

  expect_resample_result(resample(task, learner, resampling, callbacks = callback))

})

test_that("on_evaluation_before_predict works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_before_predict = function(callback, context) {
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      assert_null(context$param_values)
      assert_list(context$sets, len = 2)
      assert_names(names(context$sets), identical.to = c("train", "test"))
      assert_integer(context$sets$train)
      assert_integer(context$sets$test)
      assert_class(context$learner$model, "rpart")
      assert_null(context$test_set)
      assert_true(context$predict_sets == "test")
      assert_null(context$pdatas)
    }
  )

  expect_resample_result(resample(task, learner, resampling, callbacks = callback))
})

test_that("on_evaluation_end works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_end = function(callback, context) {
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      assert_null(context$param_values)
      assert_list(context$sets, len = 2)
      assert_names(names(context$sets), identical.to = c("train", "test"))
      assert_integer(context$sets$train)
      assert_integer(context$sets$test)
      assert_class(context$learner$model, "rpart")
      assert_null(context$test_set)
      assert_true(context$predict_sets == "test")
      assert_class(context$pdatas$test, "PredictionData")
    }
  )

  expect_resample_result(resample(task, learner, resampling, callbacks = callback))
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

  walk(rr$learners, function(learner) {
    expect_equal(learner$state$test, 1)
  })
})

test_that("writing to data_extra works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_evaluation("test",

    on_evaluation_end = function(callback, context) {
      context$data_extra$test = 1
    }
  )

  rr = resample(task, learner, resampling, callbacks = callback)

  walk(rr$data_extra, function(x) {
    expect_equal(x$test, 1)
  })
})
