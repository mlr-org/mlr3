test_that("score_measure works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = clbk("mlr3.score_measures", measures = msr("selected_features"))

  rr = resample(task, learner, resampling = resampling, callbacks = callback)

  expect_list(rr$data_extra)
  walk(rr$data_extra, function(data) {
    expect_names(names(data), must.include = "score_measures")
    expect_names(names(data[["score_measures"]]), must.include = "selected_features")
  })

  callback = clbk("mlr3.score_measures", measures = msrs(c("classif.ce", "selected_features")))

  rr = resample(task, learner, resampling = resampling, callbacks = callback)

  expect_list(rr$data_extra)
  walk(rr$data_extra, function(data) {
    expect_names(names(data), must.include = "score_measures")
    expect_names(names(data[["score_measures"]]), must.include = c("classif.ce", "selected_features"))
  })
})
