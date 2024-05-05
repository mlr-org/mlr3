task = tsk("iris")
learner = lrn("classif.featureless")
resampling = rsmp("cv", folds = 3)
rr = resample(task, learner, resampling)

test_that("resample", {
  expect_resample_result(rr)

  scores = rr$score(msr("classif.ce"))
  expect_list(scores$prediction, "Prediction")
  expect_numeric(scores$classif.ce, any.missing = FALSE)
  expect_number(rr$aggregate(msr("classif.ce")))
  learners = rr$learners
  expect_different_address(learners[[1L]], learners[[2L]])
  expect_equal(uniqueN(hashes(learners)), 1L)

  rr = rr$clone(TRUE)$filter(2:3)
  tab = as.data.table(rr)
  expect_data_table(tab, nrows = 2L)
  expect_data_table(tab, nrows = 2L)
  expect_equal(tab$iteration, 2:3)
  expect_resample_result(rr, allow_incomplete = TRUE)
})

test_that("empty RR", {
  rr = ResampleResult$new()
  expect_resample_result(rr)
})

test_that("resample with no or multiple measures", {
  for (measures in list(mlr_measures$mget(c("classif.ce", "classif.acc")), list())) {
    tab = rr$score(measures, ids = FALSE)
    expect_data_table(tab, ncols = length(mlr_reflections$rr_names) + length(measures), nrows = 3L)
    expect_set_equal(names(tab), c(mlr_reflections$rr_names, ids(measures)))
    perf = rr$aggregate(measures)
    expect_numeric(perf, any.missing = FALSE, len = length(measures), names = "unique")
    expect_equal(names(perf), unname(ids(measures)))
  }
})

test_that("as_benchmark_result.ResampleResult", {
  measures = list(msr("classif.ce"), msr("classif.acc"))
  bmr = as_benchmark_result(rr)
  expect_benchmark_result(bmr)
  expect_equal(nrow(get_private(bmr)$.data), nrow(get_private(rr)$.data))
  expect_set_equal(bmr$uhashes, rr$uhash)
  aggr = bmr$aggregate()
  expect_data_table(aggr, nrows = 1)
  expect_set_equal(bmr$uhashes, rr$uhash)
})


test_that("discarding model", {
  expect_equal(map(as.data.table(rr)$learner, "model"), vector("list", 3L))
})

test_that("inputs are cloned", {
  expect_different_address(task, get_private(rr)$.data$data$tasks$task[[1]])
  expect_different_address(learner, get_private(rr)$.data$data$learners$learner[[1]])
  expect_different_address(resampling, get_private(rr)$.data$data$resamplings$resampling[[1]])
})

test_that("memory footprint", {
  expect_equal(nrow(get_private(rr)$.data$data$learners), 1L)
  expect_equal(nrow(get_private(rr)$.data$data$tasks), 1L)
  expect_equal(nrow(get_private(rr)$.data$data$resamplings), 1L)
})

test_that("predict_type is checked", {
  task = tsk("sonar")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3L)
  measure = msr("classif.auc")
  rr = resample(task, learner, resampling)

  expect_warning(rr$score(measure), "predict type", fixed = TRUE)
  expect_warning(rr$aggregate(measure), "predict type", fixed = TRUE)
})

test_that("empty train/predict sets", {
  task = tsk("mtcars")
  learner = lrn("regr.rpart")

  expect_error(learner$train(task, integer()))

  learner$train(task)
  expect_prediction(learner$predict(task, integer()))

  resampling = rsmp("holdout", ratio = 0)
  expect_error(resample(task, learner, resampling))

  resampling = rsmp("holdout", ratio = 1)
  expect_prediction(resample(task, learner, resampling)$predictions()[[1]])
})

test_that("conditions are returned", {
  expect_true(all(c("warnings", "errors") %in% names(rr$score(conditions = TRUE))))
})

test_that("save/load roundtrip", {
  path = tempfile()
  saveRDS(rr, file = path)

  rr2 = readRDS(path)
  expect_resample_result(rr2)
})

test_that("debug branch", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 2)
  rr = invoke(resample, task = task, learner = learner, resampling = resampling, .opts = list(mlr3.debug = TRUE))
  expect_resample_result(rr)
})

test_that("encapsulation", {
  task = tsk("iris")
  learner = lrn("classif.debug", error_train = 1)
  resampling = rsmp("holdout")

  expect_error(resample(task, learner, resampling), "classif.debug->train()")

  rr = resample(task, learner, resampling, encapsulate = "evaluate")
  expect_data_table(rr$errors, nrows = 1L)
  expect_class(rr$learner$fallback, "LearnerClassifFeatureless")
  expect_equal(rr$learner$encapsulate[["train"]], "evaluate")
  expect_equal(rr$learner$encapsulate[["predict"]], "evaluate")
})

test_that("disable cloning", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("holdout")

  rr = resample(task, learner, resampling, clone = c())

  expect_same_address(task, rr$task)
  expect_same_address(learner, get_private(rr)$.data$data$learners$learner[[1]])
  expect_same_address(resampling, rr$resampling)

  expect_identical(task$hash, rr$task$hash)
  expect_identical(learner$hash, rr$learner$hash)
  expect_true(resampling$is_instantiated)
  expect_identical(resampling$hash, rr$resampling$hash)
})

test_that("as_resample_result works for result data", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("holdout")

  rr = resample(task, learner, resampling, clone = c())

  result_data = get_private(rr)$.data

  rr2 = as_resample_result(result_data)
  expect_class(rr2, "ResampleResult")
})

test_that("encapsulation triggers marshaling correctly", {
  learner1 = lrn("classif.debug", count_marshaling = TRUE, encapsulate = c(train = "callr"))
  learner2 = lrn("classif.debug", count_marshaling = TRUE, encapsulate = c(train = "none"))
  task = tsk("iris")
  resampling = rsmp("holdout")
  rr1 = resample(task, learner1, resampling, store_models = TRUE, unmarshal = FALSE)
  expect_true(rr1$learners[[1]]$marshaled)
  rr2 = resample(task, learner2, resampling, store_models = TRUE, unmarshal = FALSE)
  expect_false(rr2$learners[[1]]$marshaled)
})

test_that("parallel execution automatically triggers marshaling", {
  learner = lrn("classif.debug", count_marshaling = TRUE)
  task = tsk("iris")
  resampling = rsmp("holdout")
  rr = with_future(future::multisession, {
    resample(task, learner, resampling, store_models = TRUE, unmarshal = TRUE)
  })
  expect_equal(rr$learners[[1]]$model$marshal_count, 1)
  expect_false(rr$learners[[1]]$marshaled)
})

test_that("sequential execution does not trigger marshaling", {
  learner = lrn("classif.debug", count_marshaling = TRUE)
  task = tsk("iris")
  resampling = rsmp("holdout")
  rr = with_future(future::sequential, {
    resample(task, learner, resampling, store_models = TRUE, unmarshal = TRUE)
  })
  expect_equal(rr$learners[[1]]$model$marshal_count, 0)
})

test_that("parallel execution and callr marshal once", {
  learner = lrn("classif.debug", count_marshaling = TRUE, encapsulate = c(train = "callr"))
  task = tsk("iris")
  resampling = rsmp("holdout")
  rr = with_future(future::multisession, {
    resample(task, learner, resampling, store_models = TRUE, unmarshal = TRUE)
  })
  expect_equal(rr$learners[[1]]$model$marshal_count, 1)
  expect_false(rr$learners[[1]]$marshaled)
})

test_that("marshaling works when store_models is FALSE", {
  learner = lrn("classif.debug", count_marshaling = FALSE, encapsulate = c(train = "callr"))
  task = tsk("iris")
  resampling = rsmp("holdout")
  rr = with_future(future::multisession, {
    resample(task, learner, resampling, store_models = FALSE, unmarshal = TRUE)
  })
  expect_resample_result(rr)
  expect_true(is.null(rr$learners[[1]]$model))

  rr1 = with_future(future::sequential, {
    resample(task, learner, resampling, store_models = FALSE, unmarshal = TRUE)
  })
  expect_resample_result(rr1)
  expect_true(is.null(rr1$learners[[1]]$model))
})


test_that("unmarshal parameter is respected", {
  learner = lrn("classif.debug", count_marshaling = TRUE, encapsulate = c(train = "callr"))
  task = tsk("iris")
  resampling = rsmp("holdout")
  rr = with_future(future::multisession, {
    list(
      marshaled = resample(task, learner, resampling, store_models = TRUE, unmarshal = FALSE),
      unmarshaled = resample(task, learner, resampling, store_models = TRUE, unmarshal = TRUE)
    )
  })
  expect_false(rr$unmarshaled$learners[[1]]$marshaled)
  expect_true(rr$marshaled$learners[[1]]$marshaled)
})

test_that("ResampleResult can be (un)marshaled", {
  rr = resample(tsk("iris"), lrn("classif.debug"), rsmp("holdout"), store_models = TRUE)
  expect_false(rr$learners[[1]]$marshaled)
  rr$marshal()
  expect_true(rr$learners[[1]]$marshaled)
  rr$unmarshal()
  expect_false(rr$learners[[1]]$marshaled)

  # also works with non-marshalable learner
  rr1 = resample(tsk("iris"), lrn("classif.featureless"), rsmp("holdout"), store_models = TRUE)
  model = rr1$learners[[1]]$model
  rr1$unmarshal()
  expect_equal(rr1$learners[[1]]$model, model)
})

test_that("does not unnecessarily clone state", {
  task = tsk("iris")
  learner = R6Class("LearnerTest", inherit = LearnerClassifDebug, private = list(
    deep_clone = function(name, value) {
      if (name == "state" && !is.null(value)) {
        stop("Buggy bug bug")
      } else {
        super$deep_clone(name, value)
      }
    }
  ))$new()
  learner$train(task)
  expect_resample_result(resample(task, learner, rsmp("holdout")))
})

test_that("task hashes differ depending on whether test set is used", {
  task = tsk("iris")
  resampling = rsmp("holdout")
  learner1 = lrn("classif.debug")
  learner2 = lrn("classif.debug", validate = "test")
  rr1 = resample(task, learner1, resampling)
  rr2 = resample(task, learner2, resampling)
  expect_false(rr1$learners[[1]]$state$task_hash == rr2$learners[[1]]$state$task_hash)
})

test_that("can make predictions for inner_valid_task", {
  task = tsk("iris")
  learner = lrn("classif.debug", validate = 0.5, predict_sets = c("train", "inner_valid"))
  rr = resample(task, learner, rsmp("insample"))
  expect_equal(length(rr$predictions("inner_valid")[[1L]]$row_ids), task$nrow / 2)
  # the training ids don't incldue the inner validation ids
  expect_equal(length(rr$predictions("train")[[1L]]$row_ids), task$nrow / 2)
})

test_that("learner's validate cannot be 'test' if inner_valid_set is present", {
  # otherwise, predict_set = "inner_valid" would be ambiguous
  learner = lrn("classif.debug", validate = "test", predict_sets = c("train", "inner_valid"))
  task = tsk("iris")$divide(1)
  expect_error(resample(task, learner, rsmp("holdout")), "cannot be set to ")
})

test_that("learner's validate cannot be a ratio if inner_valid_set is present", {
  # otherwise, predict_set = "inner_valid" would be ambiguous
  learner = lrn("classif.debug", validate = 0.5, predict_sets = c("train", "inner_valid"))
  task = tsk("iris")$divide(1)
  expect_error(resample(task, learner, rsmp("holdout")), "cannot be set to")
})

test_that("inner_valid and train predictions", {
  task = tsk("iris")$divide(1:2)
  learner = lrn("classif.debug", validate = "inner_valid", predict_sets = c("train", "inner_valid", "test"))
  rr = resample(task, learner, rsmp("insample"), store_models = TRUE)
  measure_valid = msr("classif.acc")
  # this should not be necessary I think, but: https://github.com/mlr-org/mlr3/issues/1011
  measure_valid$predict_sets = "inner_valid"
  expect_equal(
    rr$score(measure_valid, predict_sets = "inner_valid")$classif.acc,
    rr$learners[[1L]]$inner_valid_scores$acc
  )

  # if valid = "test", inner_valid and test predictions are the same
  task = tsk("iris")
  learner = lrn("classif.debug", validate = "test", predict_sets = c("train", "inner_valid", "test"))
  rr2 = resample(task, learner, rsmp("holdout"))

  expect_equal(
    rr2$score(measure_valid, predict_sets = "inner_valid")$classif.acc,
    rr2$score(msr("classif.acc"), predict_sets = "test")$classif.acc
  )
  expect_equal(
    rr2$predictions("inner_valid")[[1L]]$response,
    rr2$predictions("test")[[1L]]$response
  )

  learner = lrn("classif.debug", validate = 0.9, predict_sets = c("train", "inner_valid", "test"))
  task = tsk("iris")$filter(1:10)
  rr2 = resample(task, learner, rsmp("insample"))
  expect_true(length(rr2$predictions("train")[[1L]]$row_ids) == 1L)
  expect_true(length(rr2$predictions("test")[[1L]]$row_ids) == 10L)

  rr3 = resample(task, learner, rsmp("holdout"))
  rr3$predictions("train")
  rr3$predictions("inner_valid")
  rr3$predictions("test")

  expect_true(length(rr2$predictions("train")[[1L]]$row_ids) == 1L)
  expect_true(length(rr2$predictions("test")[[1L]]$row_ids) == 10L)
})

test_that("properties are also checked on validation task", {
  task = tsk("iris")
  row = task$data(1)
  row[[1]][1] = NA
  row$..row_id = 151
  task$rbind(row)
  task$divide(151)
  learner = lrn("classif.debug", validate = "inner_valid")
  learner$properties = setdiff(learner$properties, "missings")

  expect_error(resample(task, learner, rsmp("holdout")), "missing values")
})

test_that("marshaling does not change class of learner state when reassembling", {
  rr = resample(tsk("iris"), lrn("classif.debug", encapsulate = c(train = "callr")), rsmp("holdout"), store_models = TRUE)
  expect_class(rr$learners[[1]]$state, "learner_state")
})

test_that("marshaled model is sent back, when unmarshal is FALSE, sequential exec and callr", {
  learner = lrn("classif.debug", count_marshaling = TRUE, encapsulate = c(train = "callr"))
  rr = resample(tsk("iris"), learner, rsmp("holdout"), store_models = TRUE, unmarshal = FALSE)
  expect_true(rr$learners[[1L]]$marshaled)
})

test_that("predict_set inner_valid throws error when none is available", {
  expect_error(
    resample(tsk("iris"), lrn("classif.debug", predict_sets = "inner_valid"), rsmp("holdout")),
    "Cannot set the predict_type"
  )
})

test_that("can even use inner_valid predict set on learners that don't support validation", {
  rr = resample(tsk("mtcars")$divide(1:10), lrn("regr.debug", predict_sets = "inner_valid"), rsmp("holdout"))
})
