test_that("print.Measure", {
  m = mlr_measures$get("classif.ce")
  expect_output(print(Measure))
})

test_that("assert_measure", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  m = mlr_measures$get("time_train")
  expect_class(assert_measure(m), "Measure")
  expect_class(assert_measure(m, task = task), "Measure")
})

test_that("average with micro/macro", {
  task = tsk("german_credit")
  learner = lrn("classif.featureless")
  macro = msr("classif.fbeta", id = "macro")
  micro = msr("classif.fbeta", id = "micro", average = "micro")
  with_seed(123, {
    rs = rsmp("cv", folds = 5)
    rr = resample(task, learner, rs)
  })
  aggr = rr$aggregate(list(macro, micro))

  expect_true(diff(aggr) > 0)
  expect_equal(aggr[[1]], mean(map_dbl(rr$predictions(), micro$score)))
  expect_equal(aggr[[1]], mean(map_dbl(rr$predictions(), macro$score)))
  expect_equal(aggr[[2]], micro$score(rr$prediction("test")))
  expect_equal(aggr[[2]], macro$score(rr$prediction("test")))
})

test_that("k is hashed in AIC", {
  m1 = msr("aic", k = 2)
  m2 = msr("aic", k = 3)

  expect_true(m1$hash != m2$hash)
})

test_that("can set beta for fbeta during construction", {
  # https://stackoverflow.com/questions/66883242/how-to-change-beta-value-when-using-classif-fbeta-as-a-performance-measure-in

  task = tsk("sonar")
  l = lrn("classif.rpart")
  l$train(task)
  p = l$predict(task)

  m1 = msr("classif.fbeta", beta = 1)
  m2 = msr("classif.fbeta", beta = 10)
  expect_equal(m1$param_set$values$beta, 1)
  expect_equal(m2$param_set$values$beta, 10)

  expect_gt(abs(m1$score(p) - m2$score(p)), 0.0001)
})

test_that("check_prerequisites / task_properties", {
  task = tsk("penguins")
  learner = lrn("classif.featureless", predict_type = "prob")
  p = learner$train(task)$predict(task)
  m = msr("classif.auc")

  expect_error(unname(m$score(p)), "exactly")
  expect_error(unname(p$score(m)), "exactly")
  expect_warning(m$score(p, task = task), "properties", fixed = TRUE)
  expect_warning(p$score(m, task = task), "properties", fixed = TRUE)

  rr = resample(task, learner, rsmp("holdout"))
  expect_warning(res <- rr$score(m), "properties", fixed = TRUE)
  expect_identical(res$classif.auc, NaN)
  expect_warning(res <- rr$aggregate(m), "properties", fixed = TRUE)
  expect_identical(unname(res), NaN)

  bmr = as_benchmark_result(rr)
  expect_warning(res <- bmr$score(m), "properties", fixed = TRUE)
  expect_identical(res$classif.auc, NaN)
  expect_warning(res <- bmr$aggregate(m), "properties", fixed = TRUE)
  expect_identical(res$classif.auc, NaN)
})

test_that("check_prerequisites / predict_type", {
  task = tsk("sonar")
  learner = lrn("classif.featureless")
  p = learner$train(task)$predict(task)
  m = msr("classif.auc")

  expect_identical(unname(suppressWarnings(m$score(p))), NaN)
  expect_identical(unname(suppressWarnings(p$score(m))), NaN)
  expect_warning(m$score(p, learner = learner), "predict type", fixed = TRUE)
  expect_warning(p$score(m, learner = learner), "predict type", fixed = TRUE)

  rr = resample(task, learner, rsmp("holdout"))
  expect_warning(res <- rr$score(m), "predict type", fixed = TRUE)
  expect_identical(res$classif.auc, NaN)
  expect_warning(res <- rr$aggregate(m), "predict type", fixed = TRUE)
  expect_identical(unname(res), NaN)

  bmr = as_benchmark_result(rr)
  expect_warning(res <- bmr$score(m), "predict type", fixed = TRUE)
  expect_identical(res$classif.auc, NaN)
  expect_warning(res <- bmr$aggregate(m), "predict type", fixed = TRUE)
  expect_identical(res$classif.auc, NaN)
})

test_that("check_prerequisites / predict_sets", {
  task = tsk("sonar")
  learner = lrn("classif.featureless", predict_sets = "train")
  rr = resample(task, learner, rsmp("holdout"))
  m = msr("classif.ce")

  expect_warning(res <- rr$score(m), "predict sets", fixed = TRUE)
  expect_identical(res$classif.ce, NaN)
  expect_warning(res <- rr$aggregate(m), "predict sets", fixed = TRUE)
  expect_identical(unname(res), NaN)

  bmr = as_benchmark_result(rr)
  expect_warning(res <- bmr$score(m), "predict sets", fixed = TRUE)
  expect_identical(res$classif.ce, NaN)
  expect_warning(res <- bmr$aggregate(m), "predict set", fixed = TRUE)
  expect_identical(res$classif.ce, NaN)
})

test_that("time_train works with different predict type (#832)", {
  rr = resample(tsk("iris"), lrn("classif.debug", predict_type = "prob"), rsmp("holdout"))
  res = rr$score(msr("time_train"))
  expect_number(res$time_train)
})

test_that("time_train is > 0", {
  # no runtime test on CRAN
  skip_on_cran()

  rr = resample(tsk("iris"), lrn("classif.debug"), rsmp("holdout"))
  res = rr$score(msr("time_train"))
  expect_gte(res$time_train, 0)
})

test_that("scoring fails when measure requires_model, but model is in marshaled state", {
  measure = msr("classif.acc")
  measure$properties = c(measure$properties, "requires_model")

  task = tsk("iris")
  learner = lrn("classif.debug")
  pred = learner$train(task)$predict(task)
  learner$marshal()
  expect_error(measure$score(pred, learner = learner, task = task),
    regexp = "model is in marshaled")
})

test_that("measure weights", {
  learner = lrn("classif.featureless", predict_type = "prob")
  learner$train(tsk("iris"), row_ids = 1)
  prediction_no_weights = learner$predict(tsk("iris"), row_ids = c(1, 150))
  prediction_learner_weights = learner$predict(iris_weights_learner, row_ids = c(1, 150))  # should behave the same as no weights
  prediction_measure_weights = learner$predict(iris_weights_measure, row_ids = c(1, 150))

  m = msr("classif.acc", use_weights = "use")
  expect_true("weights" %in% m$properties)

  # evaluating prediction with weights
  expect_equal(prediction_no_weights$score(m), c(classif.acc = 0.5))
  expect_equal(m$score(prediction_no_weights), 0.5)
  expect_equal(prediction_learner_weights$score(m), c(classif.acc = 0.5))
  expect_equal(m$score(prediction_learner_weights), 0.5)
  expect_equal(prediction_measure_weights$score(m), c(classif.acc = 1 / 101))
  expect_equal(m$score(prediction_measure_weights), 1 / 101)


  m$use_weights = "ignore"
  expect_equal(prediction_no_weights$score(m), c(classif.acc = 0.5))
  expect_equal(prediction_learner_weights$score(m), c(classif.acc = 0.5))
  expect_equal(prediction_measure_weights$score(m), c(classif.acc = 0.5))
  expect_equal(m$score(prediction_measure_weights), 0.5)

  m$use_weights = "error"
  expect_equal(prediction_no_weights$score(m), c(classif.acc = 0.5))
  expect_equal(prediction_learner_weights$score(m), c(classif.acc = 0.5))
  expect_error(prediction_measure_weights$score(m), "'use_weights' was set to 'error'")
  expect_error(m$score(prediction_measure_weights), "'use_weights' was set to 'error'")

  mauc = msr("classif.mauc_au1p")
  prediction_no_weights = learner$predict(tsk("iris"), row_ids = c(1, 2, 51, 52, 101, 102))
  prediction_learner_weights = learner$predict(iris_weights_learner, row_ids = c(1, 2, 51, 52, 101, 102))  # should behave the same as no weights
  prediction_measure_weights = learner$predict(iris_weights_measure, row_ids = c(1, 2, 51, 52, 101, 102))

  expect_equal(prediction_no_weights$score(mauc), c(classif.mauc_au1p = 0.5))
  expect_equal(prediction_learner_weights$score(mauc), c(classif.mauc_au1p = 0.5))
  expect_error(prediction_measure_weights$score(mauc), "cannot be evaluated with weights")
  expect_error(mauc$score(prediction_measure_weights), "cannot be evaluated with weights")

  mauc$use_weights = "ignore"
  expect_equal(prediction_measure_weights$score(mauc), c(classif.mauc_au1p = 0.5))
  expect_equal(mauc$score(prediction_measure_weights), 0.5)
  expect_error({mauc$use_weights = "use"}, "Must be element of set")

  # evaluating resampling with weights
  resampling = rsmp("custom")$instantiate(tsk("iris"),
    train_sets = list(1, 1),
    test_sets = list(c(1, 2, 51, 52, 101, 102), c(1:3, 51:53, 101:102))
  )
  rr_no_weights = resample(tsk("iris"), learner, resampling)
  rr_learner_weights = resample(iris_weights_learner, learner, resampling)
  rr_measure_weights = resample(iris_weights_measure, learner, resampling)

  m$use_weights = "use"
  expect_equal(rr_no_weights$score(m)$classif.acc, c(1 / 3, 3 / 8))
  expect_equal(rr_learner_weights$score(m)$classif.acc, c(1 / 3, 3 / 8))
  expect_equal(rr_measure_weights$score(m)$classif.acc, c(1 / 111, 3 / 233))

  m$use_weights = "ignore"
  expect_equal(rr_no_weights$score(m)$classif.acc, c(1 / 3, 3 / 8))
  expect_equal(rr_learner_weights$score(m)$classif.acc, c(1 / 3, 3 / 8))
  expect_equal(rr_measure_weights$score(m)$classif.acc, c(1 / 3, 3 / 8))

  m$use_weights = "error"
  expect_equal(rr_no_weights$score(m)$classif.acc, c(1 / 3, 3 / 8))
  expect_equal(rr_learner_weights$score(m)$classif.acc, c(1 / 3, 3 / 8))
  expect_error(rr_measure_weights$score(m), "since 'use_weights' was set to 'error'")

  mauc$use_weights = "ignore"
  expect_equal(rr_no_weights$score(mauc)$classif.mauc_au1p, c(1, 1) / 2)
  expect_equal(rr_learner_weights$score(mauc)$classif.mauc_au1p, c(1, 1) / 2)
  expect_equal(rr_measure_weights$score(mauc)$classif.mauc_au1p, c(1, 1) / 2)

  mauc$use_weights = "error"
  expect_equal(rr_no_weights$score(mauc)$classif.mauc_au1p, c(1, 1) / 2)
  expect_equal(rr_learner_weights$score(mauc)$classif.mauc_au1p, c(1, 1) / 2)
  expect_error(rr_measure_weights$score(mauc), "cannot be evaluated with weights in\n  .*Task.*since the Measure does not support\n  weights")

  # aggregating resampling with weights
  m$use_weights = "use"
  expect_equal(rr_no_weights$aggregate(m), c(classif.acc = 1 / 6 + 3 / 16))
  expect_equal(m$aggregate(rr_no_weights), c(classif.acc = 1 / 6 + 3 / 16))
  expect_equal(rr_learner_weights$aggregate(m), c(classif.acc = 1 / 6 + 3 / 16))
  expect_equal(m$aggregate(rr_learner_weights), c(classif.acc = 1 / 6 + 3 / 16))
  expect_equal(rr_measure_weights$aggregate(m), c(classif.acc = 1 / 222 + 3 / 466))
  expect_equal(m$aggregate(rr_measure_weights), c(classif.acc = 1 / 222 + 3 / 466))

  m$use_weights = "ignore"
  expect_equal(rr_no_weights$aggregate(m), c(classif.acc = 1 / 6 + 3 / 16))
  expect_equal(rr_learner_weights$aggregate(m), c(classif.acc = 1 / 6 + 3 / 16))
  expect_equal(rr_measure_weights$aggregate(m), c(classif.acc = 1 / 6 + 3 / 16))
  expect_equal(m$aggregate(rr_measure_weights), c(classif.acc = 1 / 6 + 3 / 16))

  m$use_weights = "error"
  expect_equal(rr_no_weights$aggregate(m), c(classif.acc = 1 / 6 + 3 / 16))
  expect_equal(rr_learner_weights$aggregate(m), c(classif.acc = 1 / 6 + 3 / 16))
  expect_error(rr_measure_weights$aggregate(m), "since 'use_weights' was set to 'error'")
  expect_error(m$aggregate(rr_measure_weights), "since 'use_weights' was set to 'error'")

  mauc$use_weights = "ignore"
  expect_equal(rr_no_weights$aggregate(mauc), c(classif.mauc_au1p = 0.5))
  expect_equal(rr_learner_weights$aggregate(mauc), c(classif.mauc_au1p = 0.5))
  expect_equal(rr_measure_weights$aggregate(mauc), c(classif.mauc_au1p = 0.5))
  expect_equal(mauc$aggregate(rr_measure_weights), c(classif.mauc_au1p = 0.5))

  mauc$use_weights = "error"
  expect_equal(rr_no_weights$aggregate(mauc), c(classif.mauc_au1p = 0.5))
  expect_equal(rr_learner_weights$aggregate(mauc), c(classif.mauc_au1p = 0.5))
  expect_error(rr_measure_weights$aggregate(mauc), "cannot be evaluated with weights in\n  .*Task.*since the Measure does not support\n  weights")
  expect_error(mauc$aggregate(rr_measure_weights), "cannot be evaluated with weights in\n  .*Task.*since the Measure does not support\n  weights")

  m$use_weights = "use"
  m$average = "macro_weighted"
  expect_equal(rr_no_weights$aggregate(m), c(classif.acc = 5 / 14))
  expect_equal(m$aggregate(rr_no_weights), c(classif.acc = 5 / 14))
  expect_equal(rr_learner_weights$aggregate(m), c(classif.acc = 5 / 14))
  expect_equal(m$aggregate(rr_learner_weights), c(classif.acc = 5 / 14))
  expect_equal(rr_measure_weights$aggregate(m), c(classif.acc = 5 / 455))
  expect_equal(m$aggregate(rr_measure_weights), c(classif.acc = 5 / 455))

  m$use_weights = "ignore"
  expect_equal(rr_no_weights$aggregate(m), c(classif.acc = 5 / 14))
  expect_equal(rr_learner_weights$aggregate(m), c(classif.acc = 5 / 14))
  expect_equal(rr_measure_weights$aggregate(m), c(classif.acc = 5 / 14))  # weighs by number of samples
  expect_equal(m$aggregate(rr_measure_weights), c(classif.acc = 5 / 14))

})

test_that("primary iters are respected", {
  task = tsk("sonar")
  learner = lrn("classif.rpart", predict_type = "prob")
  resampling = rsmp("cv", folds = 10)
  resampling$instantiate(task)
  get_private(resampling, ".primary_iters") = 1:2

  rr = resample(task, learner, resampling)
  m = msr("classif.acc")
  m$average = "macro"
  scores = rr$score(m)$classif.acc

  # macro aggregation
  expect_equal(unname(rr$aggregate(m)), mean(scores[1:2]))
  expect_true(unname(rr$aggregate(m)) != mean(scores))

  # micro aggregation
  pred_micro = do.call(c, rr$predictions()[1:2])
  scores = pred_micro$score(m)
  expect_equal(unname(m$score(pred_micro)), unname(scores))

  jaccard = msr("sim.jaccard")
  expect_error(rr$aggregate(jaccard), "primary_iters")
})

test_that("no predict_sets required (#1094)", {
  m = msr("internal_valid_score")
  expect_null(m$predict_sets)
  rr = resample(tsk("iris"), lrn("classif.debug", validate = 0.3, predict_sets = NULL), rsmp("holdout"))
  expect_double(rr$aggregate(m))
  expect_warning(rr$aggregate(msr("classif.ce")), "needs predict sets")
})

test_that("checks on predict_sets", {
  m = msr("classif.ce")
  expect_error({m$predict_sets = NULL}, "Must be a subset")
  expect_error({m$predict_sets = "imaginary"}, "Must be a subset")
})

test_that("measure and prediction type is checked", {
  learner = lrn("classif.rpart")
  task = tsk("pima")
  learner$train(task)
  pred = learner$predict(task)

  measure = msr("classif.logloss")
  expect_warning(measure$score(pred), "is missing predict type")
})

test_that("obs_loss works", {
  MeasureTest = R6Class("MeasureTest",
    inherit = Measure,
    public = list(
      initialize = function() {
        super$initialize("test", task_type = "classif", param_set = ps(), properties = "obs_loss")
      }
    ),
    private = list(
      .score = function(prediction, task, weights = NULL, ...) {
        2
      },

      .obs_loss = function(prediction, task, ...) {
        x = rnorm(length(prediction$row_ids))
        x - mean(x) + 2
      }
    )
  )

  measure = MeasureTest$new()
  learner = lrn("classif.rpart", predict_type = "prob")
  task = tsk("pima")
  learner$train(task)
  pred = learner$predict(task)
  obs_loss = measure$obs_loss(pred)
  expect_numeric(obs_loss, len = task$nrow, any.missing = FALSE)
})

