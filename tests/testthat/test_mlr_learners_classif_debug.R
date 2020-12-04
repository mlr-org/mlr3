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
  expect_list(new_learner$model, len = 4)
})

test_that("NA predictions", {
  task = tsk("iris")
  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "response")
  learner$train(task)
  p = learner$predict(task)
  expect_equal(sum(is.na(p$response)), 75L)

  learner = lrn("classif.debug", predict_missing = 0.5, predict_type = "prob")
  learner$train(task)
  p = learner$predict(task)
  expect_equal(sum(is.na(p$response)), 75L)
  expect_equal(is.na(p$response), apply(p$prob, 1, anyMissing))
})
