context("continue")

test_that("continue method works", {
  task = tsk("iris")
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5

  learner$train(task)
  expect_equal(learner$model$iter, 5)

  learner$param_set$values$iter = 10
  learner$continue(task)
  expect_equal(learner$model$iter, 10)

  expect_equal(learner$param_set$values$iter, learner$model$iter)

  expect_error(learner$continue(task),
    regexp = "No additional iterations provided")

  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5

  expect_error(learner$continue(task),
    regexp = "Learner does not contain a model")

  learner = lrn("classif.rpart")
  learner$train(task)

  expect_error(learner$continue(task),
    regexp = "Learner 'classif.rpart' does not support continue")

  task = tsk("iris")
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  learner$train(task)
  task$select("Petal.Length")

  expect_error(learner$continue(task),
    regexp = "Supplied task does not allow to continue training")
})

test_that("resample_continue works", {
  task = tsk("iris")
  learner = LearnerClassifDebug$new()
  learner$param_set$values$iter = 5
  resampling = rsmp("cv", folds = 3)
  rr = resample(task, learner, resampling, store_models = TRUE)

  rr$continue(10, store_models = TRUE)

  expect_equal(length(rr$learners), 3)
  map(rr$learners, function(l) {
    expect_equal(l$param_set$values$iter, 10)
    expect_class(l$model, "classif.debug_model")
    expect_equal(l$model$iter, 10)
  })
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

  bmr$continue(10, store_models = TRUE)

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
})
