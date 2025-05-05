task = tsk("iris")
learner = lrn("classif.featureless")
resampling = rsmp("cv", folds = 3)
rr = resample(task, learner, resampling)

test_that("resample", {
  expect_resample_result(rr)

  scores = rr$score(msr("classif.ce"), predictions = TRUE)
  expect_list(scores$prediction_test, "Prediction")
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
    tab = rr$score(measures, ids = FALSE, predictions = TRUE)
    expect_data_table(tab, ncols = length(mlr_reflections$rr_names) + length(learner$predict_sets) + length(measures), nrows = 3L)
    expect_set_equal(names(tab), c(mlr_reflections$rr_names, ids(measures), paste0("prediction_", learner$predict_sets)))
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
  expect_true(all(c("warnings", "errors") %chin% names(rr$score(conditions = TRUE))))
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
  expect_equal(rr$learner$encapsulation[["train"]], "evaluate")
  expect_equal(rr$learner$encapsulation[["predict"]], "evaluate")
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
  learner1 = lrn("classif.debug", count_marshaling = TRUE)
  learner1$encapsulate("callr", fallback = lrn("classif.featureless"))
  learner2 = lrn("classif.debug", count_marshaling = TRUE)
  learner2$encapsulate("none")
  task = tsk("iris")
  resampling = rsmp("holdout")
  rr1 = resample(task, learner1, resampling, store_models = TRUE, unmarshal = FALSE)
  expect_true(rr1$learners[[1]]$marshaled)
  rr2 = resample(task, learner2, resampling, store_models = TRUE, unmarshal = FALSE)
  expect_true(rr2$learners[[1]]$marshaled)
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
  learner = lrn("classif.debug", count_marshaling = TRUE)
  learner$encapsulate("callr", lrn("classif.featureless"))
  task = tsk("iris")
  resampling = rsmp("holdout")
  rr = with_future(future::multisession, {
    resample(task, learner, resampling, store_models = TRUE, unmarshal = TRUE)
  })
  expect_equal(rr$learners[[1]]$model$marshal_count, 1)
  expect_false(rr$learners[[1]]$marshaled)
})

test_that("marshaling works when store_models is FALSE", {
  learner = lrn("classif.debug", count_marshaling = FALSE)
  learner$encapsulate("callr", lrn("classif.featureless"))
  task = tsk("iris")
  resampling = rsmp("holdout")
  rr = with_future(future::multisession, {
    resample(task, learner, resampling, store_models = FALSE, unmarshal = TRUE)
  })
  expect_resample_result(rr)
  expect_null(rr$learners[[1]]$model)

  rr1 = with_future(future::sequential, {
    resample(task, learner, resampling, store_models = FALSE, unmarshal = TRUE)
  })
  expect_resample_result(rr1)
  expect_null(rr1$learners[[1]]$model)
})


test_that("unmarshal parameter is respected", {
  learner = lrn("classif.debug", count_marshaling = TRUE)
  learner$encapsulate("callr", lrn("classif.featureless"))
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

test_that("can make predictions for internal_valid_task", {
  task = tsk("iris")
  learner = lrn("classif.debug", validate = 0.5, predict_sets = c("train", "internal_valid"))
  rr = resample(task, learner, rsmp("insample"))
  expect_equal(length(rr$predictions("internal_valid")[[1L]]$row_ids), task$nrow / 2)
})

test_that("learner's validate cannot be 'test' if internal_valid_set is present", {
  # otherwise, predict_set = "internal_valid" would be ambiguous
  learner = lrn("classif.debug", validate = "test", predict_sets = c("train", "internal_valid"))
  task = tsk("iris")
  task$internal_valid_task = 1
  expect_error(resample(task, learner, rsmp("holdout")), "cannot be set to ")
})

test_that("learner's validate cannot be a ratio if internal_valid_set is present", {
  # otherwise, predict_set = "internal_valid" would be ambiguous
  learner = lrn("classif.debug", validate = 0.5, predict_sets = c("train", "internal_valid"))
  task$internal_valid_task = 1
  expect_error(resample(task, learner, rsmp("holdout")), "cannot be set to")
})

test_that("internal_valid and train predictions", {
  task = tsk("iris")
  task$internal_valid_task = 1:2
  learner = lrn("classif.debug", validate = "predefined", predict_sets = c("train", "internal_valid", "test"))
  rr = resample(task, learner, rsmp("insample"))
  measure_valid = msr("classif.acc")
  measure_valid$predict_sets = "internal_valid"
  expect_equal(
    rr$score(measure_valid)$classif.acc,
    rr$learners[[1L]]$internal_valid_scores$acc
  )

  # if valid = "test", internal_valid and test predictions are the same
  task = tsk("iris")
  learner = lrn("classif.debug", validate = "test", predict_sets = c("train", "internal_valid", "test"))
  rr2 = resample(task, learner, rsmp("holdout"))

  expect_equal(
    rr2$score(measure_valid)$classif.acc,
    rr2$score(msr("classif.acc"))$classif.acc
  )
  expect_equal(
    rr2$predictions("internal_valid")[[1L]]$response,
    rr2$predictions("test")[[1L]]$response
  )

  # train predictions include the validation data
  learner = lrn("classif.debug", validate = 0.5, predict_sets = c("train", "internal_valid", "test"))
  task = tsk("iris")$filter(1:10)
  rr2 = resample(task, learner, rsmp("insample"))
  expect_equal(length(rr2$predictions("train")[[1L]]$row_ids), 10L)

  rr3 = resample(task, learner, rsmp("holdout", ratio = 0.8))

  expect_equal(length(rr3$predictions("train")[[1L]]$row_ids), 8L)
  expect_subset(
    rr3$predictions("internal_valid")$row_ids, rr3$predictions("internal_valid")$row_ids
  )
  expect_equal(length(rr3$predictions("internal_valid")[[1L]]$row_ids), 4L)
  expect_equal(length(rr3$predictions("test")[[1L]]$row_ids), 2L)
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

  expect_error(resample(task, learner, rsmp("holdout")), "missing values")
})

test_that("marshaling does not change class of learner state when reassembling", {
  rr = resample(tsk("iris"), lrn("classif.debug"), rsmp("holdout"), encapsulate = "callr", store_models = TRUE)
  expect_class(rr$learners[[1]]$state, "learner_state")
})

test_that("marshaled model is sent back, when unmarshal is FALSE, sequential exec and callr", {
  learner = lrn("classif.debug", count_marshaling = TRUE)
  rr = resample(tsk("iris"), learner, rsmp("holdout"), store_models = TRUE, encapsulate = "callr", unmarshal = FALSE)
  expect_true(rr$learners[[1L]]$marshaled)
})

test_that("predict_set internal_valid throws error when none is available", {
  expect_error(
    resample(tsk("iris"), lrn("classif.debug", predict_sets = "internal_valid"), rsmp("holdout")),
    "Cannot set the predict_type"
  )
})

test_that("can even use internal_valid predict set on learners that don't support validation", {
  task = tsk("mtcars")
  task$internal_valid_task = 1:10
  rr = resample(task, lrn("regr.debug", predict_sets = "internal_valid"), rsmp("holdout"))
  expect_warning(rr$score(), "only predicted on sets")
})

test_that("callr during prediction triggers marshaling", {
  learner1 = lrn("classif.debug", count_marshaling = TRUE)
  learner1$encapsulate("callr", lrn("classif.featureless"))
  learner2 = lrn("classif.debug", count_marshaling = TRUE)
  learner2$encapsulate("callr", lrn("classif.featureless"))

  rr1 = with_future(future::multisession, {
    resample(tsk("iris"), learner1, rsmp("holdout"), unmarshal = FALSE, store_models = TRUE)
  })
  l1 = rr1$learners[[1L]]
  expect_true(l1$marshaled)
  expect_equal(l1$model$marshaled$marshal_count, 1L)

  rr2 = with_future(future::sequential, {
    resample(tsk("iris"), learner1, rsmp("holdout"), unmarshal = FALSE, store_models = TRUE)
  })
  l2 = rr2$learners[[1L]]
  expect_true(l2$marshaled)
  expect_equal(l2$model$marshaled$marshal_count, 1L)

  rr3 = with_future(future::multisession, {
    resample(tsk("iris"), learner2, rsmp("holdout"), unmarshal = FALSE, store_models = TRUE)
  })
  l3 = rr3$learners[[1L]]
  expect_true(l3$marshaled)
  expect_equal(l3$model$marshaled$marshal_count, 1L)

  rr4 = with_future(future::sequential, {
    resample(tsk("iris"), learner2, rsmp("holdout"), unmarshal = FALSE, store_models = TRUE)
  })
  l4 = rr4$learners[[1L]]
  expect_true(l4$marshaled)
  expect_equal(l4$model$marshaled$marshal_count, 1L)

  rr5 = with_future(future::multisession, {
    resample(tsk("iris"), learner1, rsmp("holdout"), unmarshal = TRUE, store_models = TRUE)
  })
  l5 = rr5$learners[[1L]]
  expect_false(l5$marshaled)
  expect_equal(l5$model$marshal_count, 1L)

  rr6 = with_future(future::sequential, {
    resample(tsk("iris"), learner1, rsmp("holdout"), unmarshal = TRUE, store_models = TRUE)
  })
  l6 = rr6$learners[[1L]]
  expect_false(l6$marshaled)
  expect_equal(l6$model$marshal_count, 1L)

  rr7 = with_future(future::multisession, {
    resample(tsk("iris"), learner2, rsmp("holdout"), unmarshal = TRUE, store_models = TRUE)
  })
  l7 = rr7$learners[[1L]]
  expect_false(l7$marshaled)
  expect_equal(l7$model$marshal_count, 1L)

  rr8 = with_future(future::sequential, {
    resample(tsk("iris"), learner2, rsmp("holdout"), unmarshal = TRUE, store_models = TRUE)
  })
  l8 = rr8$learners[[1L]]
  expect_false(l8$marshaled)
  expect_equal(l8$model$marshal_count, 1L)
})

test_that("obs_loss", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("cv", folds = 3)
  rr = resample(task, learner, resampling)

  tbl = rr$obs_loss()
  expect_integer(tbl$classif.ce)
})

test_that("multiple named measures", {
  rr = resample(tsk("iris"), lrn("classif.featureless"), rsmp("holdout"))
  res = rr$aggregate(c(acc = msr("classif.acc"), ce = msr("classif.ce")))
  expect_numeric(res[["classif.acc"]])
  expect_numeric(res[["classif.ce"]])
})

test_that("empty predictions", {
  rr = resample(tsk("iris"), lrn("classif.debug", validate = 0.3, predict_sets = NULL), rsmp("holdout"))
  preds = rr$predictions()
  expect_equal(preds, list(list()))
  pred = rr$prediction()
  expect_equal(pred, list())
})

test_that("resample result works with not predicted predict set", {
  learner = lrn("classif.debug", predict_sets = "train")
  task = tsk("iris")
  resampling = rsmp("holdout")

  rr = resample(task, learner, resampling)

  expect_list(rr$prediction(predict_sets = "test"), len = 0)
  expect_list(rr$predictions(predict_sets = "test"), len = 1)
  expect_list(rr$predictions(predict_sets = "test")[[1L]], len = 0)

  tab = as.data.table(rr)
  expect_list(tab$prediction, len = 1)
  expect_list(tab$prediction[[1]], len = 0)

  expect_warning({tab = rr$score(msr("classif.ce", predict_sets = "test"))}, "Measure")
  expect_equal(tab$classif.ce, NaN)
})

test_that("resample results works with no predicted predict set", {
  learner = lrn("classif.debug", predict_sets = NULL)
  task = tsk("iris")
  resampling = rsmp("holdout")

  rr = resample(task, learner, resampling)

  expect_list(rr$prediction(predict_sets = "test"), len = 0)
  expect_list(rr$predictions(predict_sets = "test"), len = 1)
  expect_list(rr$predictions(predict_sets = "test")[[1L]], len = 0)

  tab = as.data.table(rr)
  expect_list(tab$prediction, len = 1)
  expect_list(tab$prediction[[1]], len = 0)

  expect_warning({tab = rr$score(msr("classif.ce", predict_sets = "test"))}, "Measure")
  expect_equal(tab$classif.ce, NaN)
})

test_that("predict_time is 0 if no predict_set is specified", {
  learner = lrn("classif.featureless", predict_sets = NULL)
  rr = resample(task, learner, resampling)
  times = rr$score(msr("time_predict"))$time_predict
  expect_true(all(times == 0))
})

test_that("resampling instantiated on a different task throws an error", {
  task = tsk("spam")
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  expect_error(resample(tsk("pima"), lrn("classif.rpart"), resampling),
    "not instantiated")
})

test_that("resampling task row hash validation", {
  task = tsk("iris")
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  # Should work with same task
  expect_resample_result(resample(task, lrn("classif.rpart"), resampling))

  # Should fail if task is filtered
  task$filter(1:100)
  expect_error(resample(task, lrn("classif.rpart"), resampling), "not instantiated on task")
})

test_that("$score() checks for models", {
  rr = resample(tsk("mtcars"), lrn("regr.debug"), rsmp("holdout"))
  expect_error(rr$score(msr("aic")), "requires the trained model")
})

test_that("can change the threshold", {
  task = tsk("iris")$filter(1:80)$droplevels("Species")
  rr = resample(task, lrn("classif.featureless", predict_type = "prob"), rsmp("insample"))
  expect_true(all(rr$prediction()$response == "setosa"))
  rr$set_threshold(0.9)
  expect_true(all(rr$prediction()$response == "versicolor"))
  rr$set_threshold(0.1)
  expect_true(all(rr$prediction()$response == "setosa"))
  rr$set_threshold(0.625, ties_method = "first")
  expect_true(all(rr$prediction()$response == "setosa"))
  rr$set_threshold(0.625, ties_method = "last")
  expect_true(all(rr$prediction()$response == "versicolor"))
  with_seed(1, {
    rr$set_threshold(0.625, ties_method = "random")
    expect_true("setosa" %in% rr$prediction()$response && "versicolor" %in% rr$prediction()$response)
  })
})

test_that("hashes work", {
  task = tsk("spam")
  learner = lrn("classif.debug")
  resampling = rsmp("cv", folds = 3)

  rr = resample(task, learner, resampling)
  task_hashes = map(rr$learners, function(learner) learner$state$task_hash)
  expect_length(unique(task_hashes), 3)
})
