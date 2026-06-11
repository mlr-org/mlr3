test_that("LearnerClassif predict_newdata_fast response works", {
  learner = lrn("classif.debug")
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_character(pred$response)
})

test_that("LearnerClassif predict_newdata_fast prob works", {
  learner = lrn("classif.debug", predict_type = "prob")
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_matrix(pred$prob, nrows = nrow(newdata), ncols = length(task$class_names))
})

test_that("LearnerClassif predict_newdata_fast works with missing values", {
  learner = lrn("classif.debug", predict_missing = 0.5)
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless"))
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_character(pred$response, any.missing = FALSE)

  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "prob")
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless", predict_type = "prob"))
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_matrix(pred$prob, nrows = nrow(newdata), ncols = length(task$class_names), any.missing = FALSE)
})

test_that("LearnerClassif predict_newdata_fast works with failed train", {
  learner = lrn("classif.debug", predict_missing = 0.5, error_train = 1)
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless"))
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_character(pred$response, any.missing = FALSE)

  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "prob", error_train = 1)
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless", predict_type = "prob"))
  task = tsk("pima")
  newdata = task$data()
  learner$train(task)
  pred = learner$predict_newdata_fast(newdata)

  expect_list(pred)
  expect_names(names(pred), subset.of = c("response", "prob"))
  expect_matrix(pred$prob, nrows = nrow(newdata), ncols = length(task$class_names), any.missing = FALSE)
})

test_that("LearnerClassif predict_newdata_fast restores fallback state after parallel resample", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")

  task = tsk("pima")
  newdata = task$data()

  # parallel execution sends the learner across a process boundary, so the trained fallback survives
  # only in the learner state, not on the fallback object itself
  resample_parallel = function(...) {
    learner = lrn("classif.debug", ...)
    learner$encapsulate("evaluate", fallback = lrn("classif.featureless"))
    rr = with_future(future.callr::callr, resample(task, learner, rsmp("holdout"), store_models = TRUE))
    l = rr$learners[[1]]
    expect_true(is.null(l$fallback$state$model))
    l
  }

  # train fully failed -> wholesale fallback
  pred = resample_parallel(error_train = 1)$predict_newdata_fast(newdata, task)
  expect_character(pred$response, len = nrow(newdata), any.missing = FALSE)

  # partial missing predictions -> fallback imputation
  pred = resample_parallel(predict_missing = 0.5)$predict_newdata_fast(newdata, task)
  expect_character(pred$response, len = nrow(newdata), any.missing = FALSE)
})
