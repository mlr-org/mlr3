test_that("continue method works", {
    task = tsk("iris")
    learner = mlr3learners::LearnerClassifXgboost$new()
    learner$param_set$values$nrounds = 5

    learner$train(task)
    m1 = learner$model

    learner$param_set$values$nrounds = 10
    learner$continue(task)
    m2 = learner$model

    expect_equal(learner$param_set$values$nrounds, learner$model$niter)
    expect_false(identical(m1, m2))

    expect_error(learner$continue(task),
                 regexp = "No additional boosting iterations provided")

    learner = mlr3learners::LearnerClassifXgboost$new()
    learner$param_set$values$nrounds = 5

    expect_error(learner$continue(task),
                 regexp = "Learner does not contain a model")

    learner = lrn("classif.rpart")
    learner$train(task)

    expect_error(learner$continue(task),
                 regexp = "Learner 'classif.rpart' does not support continue")

    task = tsk("iris")
    learner = mlr3learners::LearnerClassifXgboost$new()
    learner$param_set$values$nrounds = 5
    learner$train(task)
    task$select("Petal.Length")

    expect_error(learner$continue(task),
                 regexp = "Supplied task does not allow to continue training")
})

test_that("resample_continue works", {

  task = tsk("iris")
  learner = mlr3learners::LearnerClassifXgboost$new()
  learner$param_set$values$nrounds = 5
  resampling = rsmp("cv", folds = 3)
  rr = resample(task, learner, resampling, store_models = TRUE)

  learner$param_set$values$nrounds = 10
  rr2 = resample_continue(learner, rr, store_models = TRUE)

  expect_equal(length(rr$learners), 3)
  map(rr2$learners, function(l) {
    expect_equal(l$param_set$values$nrounds, 10)
    expect_class(l$model, "xgb.Booster")
    expect_equal(l$model$niter, 10)
  })
  expect_false(identical(rr2$learners[[1]]$model$raw, rr2$learners[[2]]$model$raw))
  expect_false(identical(rr2$learners[[1]]$model$raw, rr2$learners[[3]]$model$raw))
  expect_false(identical(rr2$learners[[2]]$model$raw, rr2$learners[[3]]$model$raw))

})

test_that("benchmark_continue works", {
  task = tsk("iris")
  learner1 = mlr3learners::LearnerClassifXgboost$new()
  learner1$param_set$values$nrounds = 5
  learner1$param_set$values$eta = 0.3
  learner1$id = "classif.xgboost_1"

  learner2 = mlr3learners::LearnerClassifXgboost$new()
  learner2$param_set$values$nrounds = 5
  learner2$param_set$values$eta = 0.4
  learner2$id = "classif.xgboost_1"
  learners = list(learner1, learner2)
  resampling = rsmp("cv", folds = 3)


  design = benchmark_grid(task, learners, resampling)
  bmr = benchmark(design, store_models = TRUE)

  learners[[1]]$param_set$values$nrounds = 10
  learners[[2]]$param_set$values$nrounds = 10

  bmr2 = benchmark_continue(learners, bmr, store_models = TRUE)

  eta = c(0.3, 0.4)
  map(seq(bmr2$n_resample_results), function(i) {
    learners = bmr2$resample_result(i)$learners
    expect_equal(length(learners), 3)
    map(learners, function(l) {
      expect_equal(l$param_set$values$nrounds, 10)
      expect_class(l$model, "xgb.Booster")
      expect_equal(l$model$niter, 10)
      expect_equal(l$param_set$values$eta, eta[i])
    })
  })

  expect_false(identical(bmr2$resample_result(1)$learners[[1]]$model$raw,
                         bmr2$resample_result(2)$learners[[1]]$model$raw))
})
