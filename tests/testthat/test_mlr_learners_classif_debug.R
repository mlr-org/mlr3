test_that("Simple training/predict", {
  task = tsk("iris")
  learner = lrn("classif.debug")
  expect_learner(learner, task)

  prediction = learner$train(task)$predict(task)
  expect_class(learner$model, "classif.debug_model")
  expect_character(learner$model$response, len = 1L, any.missing = FALSE)
  expect_factor(prediction$response, any.missing = FALSE, levels = levels(learner$model))
})

test_that("updating model works", {
  task = tsk("iris")
  learner = lrn("classif.debug", save_tasks = TRUE)
  learner$train(task, 1:10)
  expect_task(learner$model$task_train)
  prediction = learner$predict(task, row_ids = 11:20)
  expect_task(learner$model$task_predict)

  itrain = task$clone(TRUE)$filter(1:10)
  ipredict = task$clone(TRUE)$filter(11:20)

  expect_equal(hashes(learner$model[c("task_train", "task_predict")]), hashes(list(itrain, ipredict)))
})

test_that("updating model works / resample", {
  learner = lrn("classif.debug", save_tasks = TRUE)
  rr = resample(tsk("iris"), learner, rsmp("holdout"), store_models = TRUE)
  new_learner = rr$learners[[1]]
  expect_list(new_learner$model, len = 6)
})

test_that("NA predictions", {
  task = tsk("iris")
  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "response")
  learner$train(task)
  p = learner$predict(task)
  expect_equal(count_missing(p$response), 75L)

  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "prob")
  learner$train(task)
  p = learner$predict(task)
  expect_equal(count_missing(p$response), 75L)
  expect_equal(is.na(p$response), apply(p$prob, 1, anyMissing))
})

test_that("early_stopping set is available", {
  task = tsk("iris")
  task$set_row_roles(1:10, roles = "early_stopping")
  learner = lrn("classif.debug", save_tasks = TRUE)
  learner$train(task)

  row_roles = learner$model$task_train$row_roles
  expect_names(names(row_roles), permutation.of = c("use", "holdout", "early_stopping"))
  expect_equal(row_roles$early_stopping, seq(10))

  rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)

  walk(seq(rr$iters), function(i) {
    expect_equal(rr$learners[[i]]$model$task_train$row_roles$early_stopping, seq(10))
  })
})

test_that("default_values", {
  learner = lrn("classif.debug")
  search_space = ps(iter = p_int(1, 10))
  task = tsk("pima")

  values = default_values(learner, search_space, task)
  expect_names(names(values), identical.to = "iter")
})

test_that("default_values works with empty search space", {
  learner = lrn("classif.debug")
  expect_list(default_values(learner, ps(), task), len = 0)
})
