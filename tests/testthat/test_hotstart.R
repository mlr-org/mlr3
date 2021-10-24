test_that("learner is hotstarted when the hotstart parameter is increased", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  id = learner_1$model$id

  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStack$new(list(learner_1))
  learner$hotstart_stack = hot
  learner$train(task)

  expect_equal(learner$model$id, id)
  expect_equal(learner$model$iter, 2)
  expect_equal(learner$param_set$values$iter, 2)
})

test_that("learner is directly returned when hotstart and target learner are equal", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  id = learner_1$model$id

  learner = lrn("classif.debug", iter = 1)
  hot = HotstartStack$new(list(learner_1))
  learner$hotstart_stack = hot
  learner$train(task)

  expect_equal(learner$model$id, id)
})

test_that("learner is trained when target learner has an additional non-hotstart parameter", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  id = learner_1$model$id

  learner = lrn("classif.debug", iter = 2, x = 1)
  hot = HotstartStack$new(list(learner_1))
  learner$hotstart_stack = hot
  learner$train(task)

  expect_true(learner$model$id != id)
  expect_equal(learner$param_set$values$x, 1)
  expect_equal(learner$param_set$values$iter, 2)
  expect_equal(learner$model$iter, 2)
})

test_that("learner is trained when target learner has an additional hotstart parameter", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", x = 0)
  learner_1$train(task)
  learner_1$state$param_vals$iter = NULL # iter set by default. Assume it is not.
  id = learner_1$model$id

  learner = lrn("classif.debug", iter = 5)
  hot = HotstartStack$new(list(learner_1))
  learner$hotstart_stack = hot
  learner$train(task)

  expect_true(learner$model$id != id)
})

test_that("learner is trained when target learner has an increased hotstart parameter and additional non-hotstart parameter", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  id = learner_1$model$id

  learner = lrn("classif.debug", iter = 2, x = 1)
  hot = HotstartStack$new(list(learner_1))
  learner$hotstart_stack = hot
  learner$train(task)

  expect_true(learner$model$id != id)
})

test_that("learner is trained when the hotstart parameter of the target and hotstart learner are equal but the non-hotstart parameter is changed", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1, x = 0)
  learner_1$train(task)
  id = learner_1$model$id

  learner = lrn("classif.debug", iter = 1, x = 1)
  hot = HotstartStack$new(list(learner_1))
  learner$hotstart_stack = hot
  learner$train(task)

  expect_true(learner$model$id != id)
})

test_that("learner is hotstarted when the non-hotstart parameter of the target and hotstart learner are equal but the hotstart parameter is increased", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1, x = 0)
  learner_1$train(task)
  id = learner_1$model$id

  learner = lrn("classif.debug", iter = 2, x = 0)
  hot = HotstartStack$new(list(learner_1))
  learner$hotstart_stack = hot
  learner$train(task)

  expect_equal(learner$model$id, id)
})

test_that("learner is trained when the task is different", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  id = learner_1$model$id

  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStack$new(list(learner_1))
  learner$hotstart_stack = hot
  learner$train(tsk("iris"))

  expect_true(learner$model$id != id)
})

test_that("learner is trained when the stack is empty", {
  task = tsk("pima")
  learner = lrn("classif.debug", iter = 2)
  learner$hotstart_stack = HotstartStack$new()
  learner$train(tsk("iris"))

  expect_class(learner$model, "classif.debug_model")
})

test_that("learners are hotstarted when resample is used", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  rr = resample(task, learner_1, resampling, store_models = TRUE)

  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStack$new(rr$learners)
  learner$hotstart_stack = hot

  rr_2 = resample(task, learner, resampling, store_models = TRUE, allow_hotstart = TRUE)
  pmap(list(rr$learners, rr_2$learners), function(l1, l2) {
    expect_equal(l2$param_set$values$iter, 2)
    expect_class(l2$model, "classif.debug_model")
    expect_equal(l2$model$iter, 2)
    expect_equal(l1$model$id, l2$model$id)
    expect_null(l2$hotstart_stack)
  })
})

test_that("learners are hotstarted when benchmark is called", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_2 = lrn("classif.debug", iter = 2)
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  design = benchmark_grid(task, list(learner_1, learner_2), resampling)
  bmr = benchmark(design, store_models = TRUE)

  learners = unlist(map(seq_len(bmr$n_resample_results), function(i) bmr$resample_result(i)$learners))
  hot = HotstartStack$new(learners)
  ids = map_chr(learners, function(l) l$model$id)
})

test_that("learners are trained when benchmark is called but no hotstartable models are found", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_2 = lrn("classif.debug", iter = 2)
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  design = benchmark_grid(task, list(learner_1, learner_2), resampling)
  bmr = benchmark(design, store_models = TRUE)

  learners = unlist(map(seq_len(bmr$n_resample_results), function(i) bmr$resample_result(i)$learners))
  hot = HotstartStack$new(learners)

  learner = lrn("classif.debug", iter = 3)
  learner$hotstart_stack = hot

  design = benchmark_grid(task, learner, resampling)
  bmr_2 = benchmark(design, store_models = TRUE, allow_hotstart = TRUE)

  map(bmr_2$resample_result(1)$learners, function(l1) {
    expect_equal(l1$param_set$values$iter, 3)
    expect_class(l1$model, "classif.debug_model")
    expect_equal(l1$model$iter, 3)
    expect_true(l1$model$id %in% ids)
    expect_null(l1$hotstart_stack)
  })
})

test_that("learners are trained and hotstarted when benchmark is called", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_2 = lrn("classif.debug", iter = 2)
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  design = benchmark_grid(task, list(learner_1, learner_2), resampling)
  bmr = benchmark(design, store_models = TRUE)

  learners = unlist(map(seq_len(bmr$n_resample_results), function(i) bmr$resample_result(i)$learners))
  hot = HotstartStack$new(learners)

  learner_3 = lrn("classif.debug", iter = 2)
  learner_3$hotstart_stack = hot
  learner_4 = lrn("classif.rpart")
  learner_4$hotstart_stack = hot

  design = benchmark_grid(task, list(learner_3, learner_4), resampling)
  bmr_2 = benchmark(design, store_models = TRUE, allow_hotstart = TRUE)
})

test_that("learners are cloned when hotstarting is applied", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  resampling = rsmp("holdout")
  resampling$instantiate(task)

  design = benchmark_grid(task, learner_1, resampling)
  bmr  = benchmark(design, store_models = TRUE, allow_hotstart = TRUE)

  learner_2 = lrn("classif.debug", iter = 2)
  hot = HotstartStack$new(bmr$resample_result(1)$learners)
  learner_2$hotstart_stack = hot

  design = benchmark_grid(task, learner_2, resampling)
  bmr  = benchmark(design, store_models = TRUE, allow_hotstart = TRUE)

  expect_equal(bmr$resample_result(1)$learners[[1]]$param_set$values$iter, 2)
  expect_equal(bmr$resample_result(1)$learners[[1]]$model$iter, 2)
  expect_equal(hot$stack$start_learner[[1]]$param_set$values$iter, 1)
  expect_equal(hot$stack$start_learner[[1]]$model$iter, 1)
  expect_equal(bmr$resample_result(1)$learners[[1]]$model$id,
    hot$stack$start_learner[[1]]$model$id)
})
