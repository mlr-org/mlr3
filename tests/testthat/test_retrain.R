test_that("Learner$retrain() and Learner$is_retrainable() method work", {
  task = tsk("iris")

  # increased retrain parameter
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$train(task)
  expect_equal(learner$model$iter, 5)
  retrain_id = learner$model$retrain_id

  learner$retrain(task, list(iter = 10))
  expect_equal(learner$model$retrain_id, retrain_id)
  expect_equal(learner$model$iter, 10)
  expect_equal(learner$param_set$values$iter, 10)

  # equal retrain parameter
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$train(task)
  retrain_id = learner$model$retrain_id

  param_vals = list(iter = 5)
  expect_false(learner$is_retrainable(param_vals))
  learner$retrain(task, param_vals)
  expect_true(retrain_id != learner$model$retrain_id)
  expect_equal(learner$param_set$values$iter, 5)

  # added non-retrain parameter
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$train(task)
  retrain_id = learner$model$retrain_id

  learner$retrain(task, list(x = 1))
  expect_true(retrain_id != learner$model$retrain_id)
  expect_equal(learner$param_set$values$x, 1)
  expect_equal(learner$param_set$values$iter, 5)
  expect_equal(learner$model$iter, 5)

  # added retrain parameter
  learner = LearnerClassifDebug$new()
  learner$param_set$values$x = 0
  learner$train(task)
  learner$state$param_vals$iter = NULL # iter set by default. Assume it is not.

  expect_false(learner$is_retrainable(list(iter = 5)))
  learner$retrain(task, list(iter = 5))
  expect_true(retrain_id != learner$model$retrain_id)
  expect_equal(learner$param_set$values$x, 0)
  expect_equal(learner$param_set$values$iter, 5)
  expect_equal(learner$model$iter, 5)

  # increased retrain and added non-retrain parameter
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$train(task)
  retrain_id = learner$model$retrain_id

  param_vals = list(x = 1, iter = 10)
  expect_false(learner$is_retrainable(param_vals))
  learner$retrain(task, param_vals)
  expect_true(retrain_id != learner$model$retrain_id)
  expect_equal(learner$param_set$values$x, 1)
  expect_equal(learner$param_set$values$iter, 10)

  # equal retrain and changed non-retrain parameter
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$param_set$values$x = 0
  learner$train(task)
  retrain_id = learner$model$retrain_id

  param_vals = list(x = 1, iter = 5)
  expect_false(learner$is_retrainable(param_vals))
  learner$retrain(task, param_vals)
  expect_true(retrain_id != learner$model$retrain_id)
  expect_equal(learner$param_set$values$x, 1)
  expect_equal(learner$param_set$values$iter, 5)

  # increased retrain and equal non-retrain parameter
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$param_set$values$x = 0
  learner$train(task)
  retrain_id = learner$model$retrain_id

  param_vals = list(x = 0, iter = 10)
  expect_true(learner$is_retrainable(param_vals))
  learner$retrain(task, param_vals)
  expect_equal(learner$model$retrain_id, retrain_id)
  expect_equal(learner$param_set$values$x, 0)
  expect_equal(learner$param_set$values$iter, 10)

  # equal retrain parameter and disallow train
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$train(task)
  expect_error(learner$retrain(task, list(iter = 5), allow_train = FALSE),
    regexp = "<LearnerClassifDebug:classif.debug> is not retrainable.",
    fixed = TRUE)

  # retrain no model
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  expect_false(learner$is_retrainable(list(iter = 5)))
  learner$retrain(task, list(iter = 5))
  expect_equal(learner$param_set$values$iter, 5)

  # param not in param set
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$train(task)

  expect_error(learner$retrain(task, list(y = 1)),
    regexp = "Assertion on 'param_vals' failed: Parameter 'y' not available.",
    fixed = TRUE)

  # different task
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$train(task)
  task$select("Petal.Length")

  expect_error(learner$retrain(task, list(iter = 10)),
    regexp = "Assertion on 'task$feature_names' failed: Must be a permutation of set {Petal.Length,Petal.Width,Sepal.Length,Sepal.Width}.",
    fixed = TRUE)
})

test_that("Learner$which_retrain() method works", {
  task = tsk("iris")

  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 4
  learner$train(task)

  xss = list(list(iter = 1), list(iter = 2))
  expect_equal(learner$which_retrain(xss), 2)

  xss = list(list(iter = 2), list(iter = 6))
  expect_equal(learner$which_retrain(xss), 1)

  xss = list(list(iter = 6), list(iter = 8))
  expect_equal(learner$which_retrain(xss), integer())

  xss = list(list(iter = 2), list(iter = 4))
  expect_equal(learner$which_retrain(xss), 1)
})

test_that("ResampleResult$retrain() works", {
  task = tsk("iris")
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  resampling = rsmp("cv", folds = 3)
  rr = resample(task, learner, resampling, store_models = TRUE)

  rr$retrain(list(iter = 10), store_models = TRUE)

  expect_equal(length(rr$learners), 3)
  map(rr$learners, function(l) {
    expect_equal(l$param_set$values$iter, 10)
    expect_class(l$model, "classif.debug_model")
    expect_equal(l$model$iter, 10)
  })

  expect_false(rr$is_retrainable(list(iter = 10)))
})

test_that("benchmark with retrain works", {
  task = tsk("iris")
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  resampling = rsmp("cv", folds = 3)
  design = benchmark_grid(task, learner, resampling)
  bmr = benchmark(design, store_models = TRUE)

  # only retrain
  learner$param_set$values$iter = 10
  resampling$instantiate(task)
  design = data.table(task = list(task),
    learner = list(learner),
    resampling = list(resampling),
    retrain = list(bmr$resample_result(1)$learners))
  bmr_2 = benchmark(design, store_models = TRUE)

  expect_equal(bmr_2$n_resample_results, 1)
  expect_equal(length(bmr_2$resample_result(1)$learners), 3)
  map(bmr_2$resample_result(1)$learners, function(l) {
    expect_equal(l$param_set$values$iter, 10)
    expect_class(l$model, "classif.debug_model")
    expect_equal(l$model$iter, 10)
  })

  # train and retrain mixed
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  resampling = rsmp("cv", folds = 3)

  design = benchmark_grid(task, list(learner, learner), resampling)
  bmr = benchmark(design, store_models = TRUE)
  retrain_id_1_1 = map(bmr$resample_result(1)$learners, function(l) l$model$retrain_id)
  retrain_id_1_2 = map(bmr$resample_result(2)$learners, function(l) l$model$retrain_id)

  learner$param_set$values$iter = 10
  resampling$instantiate(task)
  design = data.table(task = list(task),
    learner = list(learner),
    resampling = list(resampling),
    retrain = list(list(), bmr$resample_result(2)$learners))
  bmr_2 = benchmark(design, store_models = TRUE)

  retrain_id_2_1 = map(bmr_2$resample_result(1)$learners, function(l) l$model$retrain_id)
  retrain_id_2_2 = map(bmr_2$resample_result(2)$learners, function(l) l$model$retrain_id)

  expect_equal(retrain_id_1_2, retrain_id_2_2)
  expect_true(all(unlist(retrain_id_1_1) != unlist(retrain_id_2_1)))

  # not retrainable
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  resampling = rsmp("cv", folds = 3)
  design = benchmark_grid(task, learner, resampling)
  bmr = benchmark(design, store_models = TRUE)

  resampling$instantiate(task)
  design = data.table(task = list(task),
    learner = list(lrn("classif.rpart")),
    resampling = list(resampling),
    retrain = list(bmr$resample_result(1)$learners))

  expect_error(benchmark(design, store_models = TRUE),
    regexp = "Assertion on 'param_vals' failed")
})
