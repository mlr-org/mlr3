test_that("score_measure works", {
  task = tsk("pima")
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)

  callback = clbk("mlr3.score_measures", measures = msr("selected_features"))

  rr = resample(task, learner, resampling = resampling, callbacks = callback)

  walk(rr$learners, function(learner) {
    expect_number(learner$state$selected_features)
  })
})
