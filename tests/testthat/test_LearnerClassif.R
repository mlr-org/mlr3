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

test_that("LearnerClassif predict_newdata_fast syncs fallback predict_type", {
  task = tsk("pima")
  newdata = task$data()

  # main learner predicts "prob" while the fallback is left at its default "response":
  # the predict type is only synced in the predict path, so the fallback object stays unsynced after train
  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "prob")
  expect_warning(learner$encapsulate("evaluate", fallback = lrn("classif.featureless")), "different predict types")
  learner$train(task)
  expect_equal(learner$fallback$predict_type, "response")

  pred = learner$predict_newdata_fast(newdata)
  expect_matrix(pred$prob, nrows = nrow(newdata), ncols = length(task$class_names), any.missing = FALSE)
})

test_that("LearnerClassif predict_newdata_fast aligns fallback prob columns by name", {
  # target levels are not in appearance order, so the fallback (orders columns by appearance) and the main
  # learner (orders by level) produce prob matrices with different column orders; a positional substitution
  # would silently report each class's probability under the wrong class name
  set.seed(1)
  dt = data.table(x = rnorm(100), y = factor(c(rep("a", 70), rep("b", 20), rep("c", 10)), levels = c("c", "b", "a")))
  task = TaskClassif$new("align", dt, target = "y")

  prior = lrn("classif.featureless", predict_type = "prob")$train(task)$predict_newdata_fast(dt[1])$prob[1, ]

  # predict_missing = 1 imputes every row with the fallback, so the whole prob matrix must equal the priors
  learner = lrn("classif.debug", predict_type = "prob", predict_missing = 1)
  learner$encapsulate("evaluate", fallback = lrn("classif.featureless", predict_type = "prob"))
  learner$train(task)
  pred = learner$predict_newdata_fast(dt)

  expect_equal(pred$prob[1, names(prior)], prior)
})
