# Callback
callback = callback_resample(
  id = "test",
  on_resample_before_result_data = function(callback, context) {
    print("on_resample_before_result_data")
  }
)

learner = lrn("classif.rpart")
task = tsk("iris")
resampling = rsmp("cv", folds = 3)

resample(task, learner, resampling, callbacks = callback)
