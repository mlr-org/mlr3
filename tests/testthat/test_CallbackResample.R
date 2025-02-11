# stages -----------------------------------------------------------------------

test_that("on_resample_begin works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_resample("test",

    on_resample_begin = function(callback, context) {
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

test_that("on_resample_before_train works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_resample("test",

    on_resample_before_train = function(callback, context) {
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      assert_number(context$iteration)
      assert_null(context$pdatas)
    }
  )

  expect_resample_result(resample(task, learner, resampling, callbacks = callback))

})

test_that("on_resample_before_predict works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_resample("test",

    on_resample_before_predict = function(callback, context) {
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      assert_null(context$pdatas)
    }
  )

  expect_resample_result(resample(task, learner, resampling, callbacks = callback))
})

test_that("on_resample_end works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_resample("test",

    on_resample_end = function(callback, context) {
      assert_task(context$task)
      assert_learner(context$learner)
      assert_resampling(context$resampling)
      assert_number(context$iteration)
      assert_class(context$pdatas$test, "PredictionData")
    }
  )

  expect_resample_result(resample(task, learner, resampling, callbacks = callback))
})

# learner state ----------------------------------------------------------------

test_that("writing to learner$state works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_resample("test",
    on_resample_end = function(callback, context) {
      context$learner$state$test = 1
    }
  )

  # resample result
  rr = resample(task, learner, resampling, callbacks = callback)
  walk(rr$learners, function(learner) {
    expect_equal(learner$state$test, 1)
  })
  expect_data_table(rr$data_extra, nrows = 3)
  walk(rr$data_extra$data_extra, expect_null)
  tab = as.data.table(rr)
  expect_names(names(tab), disjunct.from = "data_extra")

  # benchmark result
  design = benchmark_grid(task, learner, resampling)
  bmr = benchmark(design, callbacks = callback)
  walk(bmr$score()$learner, function(learner) {
    expect_equal(learner$state$test, 1)
  })
  tab = as.data.table(bmr)
  expect_names(names(tab), disjunct.from = "data_extra")
})

# data_extra -------------------------------------------------------------------

test_that("writing to data_extra works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_resample("test",
    on_resample_end = function(callback, context) {
      context$data_extra$test = 1
    }
  )

  # resample result
  rr = resample(task, learner, resampling, callbacks = callback)
  walk(rr$data_extra$data_extra, function(x) {
    expect_equal(x$test, 1)
  })

  # resample result data.table
  tab = as.data.table(rr)
  expect_data_table(tab)
  expect_names(names(tab), must.include = "data_extra")
  walk(tab$data_extra, function(x) {
    expect_equal(x$test, 1)
  })

  # benchmark data.table
  design = benchmark_grid(task, learner, resampling)
  bmr = benchmark(design, callbacks = callback)
  tab = as.data.table(bmr)
  expect_names(names(tab), must.include = "data_extra")
  expect_list(tab$data_extra)
  walk(tab$data_extra, function(x) {
    expect_equal(x$test, 1)
  })
})

test_that("data_extra is a list column", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")

  callback = callback_resample("test",
    on_resample_end = function(callback, context) {
      context$data_extra$test = 1
    }
  )

  rr = resample(task, learner, resampling, callbacks = callback)
  expect_list(as.data.table(rr)$data_extra, len = 1)
  expect_list(as.data.table(rr)$data_extra[[1]], len = 1)

  resampling = rsmp("cv", folds = 3)
  rr = resample(task, learner, resampling, callbacks = callback)
  expect_list(as.data.table(rr)$data_extra, len = 3)
  expect_list(as.data.table(rr)$data_extra[[1]], len = 1)
})

test_that("data_extra is null", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = callback_resample("test",
    on_resample_end = function(callback, context) {
      context$learner$state$test = 1
    }
  )

  # resample result
  rr = resample(task, learner, resampling, callbacks = callback)
  expect_data_table(rr$data_extra, nrows =  3)
  walk(rr$data_extra$data_extra, expect_null)

  # resample result data.table
  tab = as.data.table(rr)
  expect_data_table(tab)
  expect_names(names(tab), disjunct.from = "data_extra")
  walk(tab$data_extra, expect_null)

  # benchmark data.table
  design = benchmark_grid(task, learner, resampling)
  bmr = benchmark(design, callbacks = callback)
  tab = as.data.table(bmr)
  expect_data_table(tab)
  expect_names(names(tab), disjunct.from = "data_extra")
  walk(tab$data_extra, expect_null)
})

test_that("learner cloning in workhorse is passed to context", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("holdout")

  callback = callback_resample("test",
    on_resample_begin = function(callback, context) {
      callback$state$address_1 = data.table::address(context$learner)
    },

    on_resample_before_train = function(callback, context) {
      callback$state$address_2 = data.table::address(context$learner)
    },

    on_resample_end = function(callback, context) {
      context$data_extra = list(
        address_1 = callback$state$address_1,
        address_2 = callback$state$address_2
      )
    }
  )

  rr = resample(task, learner, resampling, callbacks = callback)

  expect_true(rr$data_extra$data_extra[[1]]$address_1 != rr$data_extra$data_extra[[1]]$address_2)
})

test_that("returning data_extra sometimes works ", {
  learners = lrns(c("classif.rpart", "classif.featureless"))
  task = tsk("pima")
  resampling = rsmp("cv", folds = 3)

  callback = callback_resample("test",
    on_resample_end = function(callback, context) {
      if (context$learner$id == "classif.featureless") {
        context$data_extra$test = 1
      }
    }
  )

  design = benchmark_grid(task, learners, resampling)
  bmr = benchmark(design, callbacks = callback)

  tab = as.data.table(bmr)
  expect_data_table(tab, nrows = 6)
  expect_names(names(tab), must.include = "data_extra")
  expect_list(tab$data_extra)
  expect_equal(sum(map_lgl(tab$data_extra, is.null)), 3)
  expect_data_table(get_private(bmr)$.data$data$data_extras, nrows = 3)
})
