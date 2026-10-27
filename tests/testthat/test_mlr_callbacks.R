test_that("model extractor works", {
  task = tsk("sonar")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  selected_features = function(learner) list(selected_features = learner$selected_features())
  callback = clbk("mlr3.model_extractor", fun = selected_features)

  rr = resample(task, learner, resampling = resampling, callbacks = callback)

  expect_list(rr$data_extra$data_extra)
  walk(rr$data_extra$data_extra, function(data) {
    expect_names(names(data), must.include = "selected_features")
    expect_subset(data[["selected_features"]], task$feature_names)
  })
})

test_that("model extractor receives an unmarshaled model", {
  task = tsk("iris")
  learner = lrn("classif.debug", count_marshaling = TRUE)
  callback = clbk("mlr3.model_extractor", fun = function(learner) list(marshaled = is_marshaled_model(learner$model)))

  rr = resample(task, learner, rsmp("holdout"), callbacks = callback, unmarshal = FALSE)
  expect_false(rr$data_extra$data_extra[[1L]]$marshaled)

  rr = resample(task, learner, rsmp("holdout"), callbacks = callback, unmarshal = FALSE, store_models = TRUE)
  expect_false(rr$data_extra$data_extra[[1L]]$marshaled)
  expect_true(is_marshaled_model(rr$learners[[1L]]$model))

  learner$encapsulate("callr", lrn("classif.featureless"))
  rr = resample(task, learner, rsmp("holdout"), callbacks = callback, store_models = TRUE)
  expect_false(rr$data_extra$data_extra[[1L]]$marshaled)
  expect_false(is_marshaled_model(rr$learners[[1L]]$model))
})

test_that("holdout task works", {
  task = tsk("sonar")
  task_holdout = task$clone()
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)
  splits = partition(task, 0.7)

  task$filter(splits$train)
  task_holdout$filter(splits$test)

  callback = clbk("mlr3.holdout_task", task = task_holdout)

  rr = resample(task, learner, resampling = resampling, callbacks = callback)

  expect_list(rr$data_extra$data_extra)
  walk(rr$data_extra$data_extra, function(data) {
    expect_names(names(data), must.include = "prediction_holdout")
    expect_prediction(data[["prediction_holdout"]])
  })
})
