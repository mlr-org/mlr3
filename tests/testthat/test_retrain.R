test_that("retrain method works", {
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

test_that("benchmark_continue works", {
  task = tsk("iris")
  learner1 = LearnerClassifDebug$new()
  learner1$param_set$values$iter = 5
  learner1$param_set$values$x = 0.3
  learner1$id = "classif.debug_model_1"

  learner2 = LearnerClassifDebug$new()
  learner2$param_set$values$iter = 5
  learner2$param_set$values$x = 0.4
  learner2$id = "classif.debug_model_2"
  learners = list(learner1, learner2)
  resampling = rsmp("cv", folds = 3)

  design = benchmark_grid(task, learners, resampling)
  bmr = benchmark(design, store_models = TRUE)

  bmr$retrain(list(iter = 10), store_models = TRUE)

  x = c(0.3, 0.4)
  map(seq(bmr$n_resample_results), function(i) {
    learners = bmr$resample_result(i)$learners
    expect_equal(length(learners), 3)
    map(learners, function(l) {
      expect_equal(l$param_set$values$iter, 10)
      expect_class(l$model, "classif.debug_model")
      expect_equal(l$model$iter, 10)
      expect_equal(l$param_set$values$x, x[i])
    })
  })

  expect_true(all(!bmr$is_retrainable(list(iter = 10))))

  # retrain and train mixed
  task = tsk("iris")
  learner1 = LearnerClassifDebug$new()
  learner1$param_set$values$iter = 5
  learner1$param_set$values$x = 0.3
  learner1$id = "classif.debug_model_1"

  learner2 = LearnerClassifDebug$new()
  learner2$param_set$values$iter = 10
  learner2$param_set$values$x = 0.4
  learner2$id = "classif.debug_model_2"
  learners = list(learner1, learner2)
  resampling = rsmp("cv", folds = 3)

  design = benchmark_grid(task, learners, resampling)
  bmr = benchmark(design, store_models = TRUE)
  retrain_id_1 = bmr$resample_result(1)$learners[[1]]$model$retrain_id 
  retrain_id_2 = bmr$resample_result(2)$learners[[1]]$model$retrain_id 

  expect_equal(bmr$is_retrainable(list(iter = 10)), c(TRUE, FALSE))
  bmr$retrain(list(iter = 10), store_models = TRUE)
  
  expect_equal(retrain_id_1, bmr$resample_result(1)$learners[[1]]$model$retrain_id)
  expect_true(retrain_id_2 != bmr$resample_result(2)$learners[[1]]$model$retrain_id)
})
