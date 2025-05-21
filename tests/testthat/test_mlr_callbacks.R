test_that("model extractor works", {
  task = tsk("pima")
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

test_that("holdout task works", {
  task = tsk("pima")
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
