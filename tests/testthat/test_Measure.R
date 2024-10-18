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

test_that("time_train is > 0", {
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
  expect_error(measure$score(pred, learner = learner),
    regexp = "is in marshaled form")
})

test_that("primary iters are respected", {
  task = tsk("sonar")
  resampling = rsmp("cv")$instantiate(task)
  train_sets = map(1:10, function(i) resampling$train_set(i))
  test_sets = map(1:10, function(i) resampling$train_set(i))
  r1 = rsmp("custom")$instantiate(task, train_sets = train_sets, test_sets = test_sets)
  get_private(r1, ".primary_iters") = 1:2
  r2 = rsmp("custom")$instantiate(task, train_sets = train_sets[1:2], test_sets = test_sets[1:2])
  r3 = rsmp("custom")$instantiate(task, train_sets = train_sets, test_sets = test_sets)

  learner = lrn("classif.rpart", predict_type = "prob")

  rr1 = resample(task, learner, r1, store_models = TRUE)
  rr2 = resample(task, learner, r2, store_models = TRUE)
  rr3 = resample(task, learner, r3, store_models = TRUE)

  m = msr("classif.acc")
  m$average = "macro"
  expect_equal(rr1$aggregate(), rr2$aggregate())
  m$average = "micro"
  expect_equal(rr1$aggregate(), rr2$aggregate())

  jaccard = msr("sim.jaccard")
  expect_error(rr1$aggregate(jaccard), "primary_iters")
  expect_error(rr2$aggregate(jaccard), NA)
  jaccard$properties = c(jaccard$properties, "primary_iters")
  x1 = rr1$aggregate(jaccard)
  x2 = rr3$aggregate(jaccard)
  expect_equal(x1, x2)
})

test_that("no predict_sets required (#1094)", {
  m = msr("internal_valid_score")
  expect_equal(m$predict_sets, NULL)
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

