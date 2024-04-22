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

test_that("marshaling does not change class of learner state when reassembling", {
  rr = resample(tsk("iris"), lrn("classif.debug", encapsulate = c(train = "callr")), rsmp("holdout"), store_models = TRUE)
  expect_class(rr$learners[[1]]$state, "learner_state")
})

test_that("marshaled model is sent back, when unmarshal is FALSE, sequential exec and callr", {
  learner = lrn("classif.debug", count_marshaling = TRUE, encapsulate = c(train = "callr"))
  rr = resample(tsk("iris"), learner, rsmp("holdout"), store_models = TRUE, unmarshal = FALSE)
  expect_true(rr$learners[[1L]]$marshaled)
})
