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
      assert_number(context$iteration)
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
      assert_number(context$iteration)
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
      assert_number(context$iteration)
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
