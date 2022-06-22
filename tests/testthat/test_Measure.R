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

  expect_identical(unname(m$score(p)), NaN)
  expect_identical(unname(p$score(m)), NaN)
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
