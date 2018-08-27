runExperiment = function(task, learner, train_set, test_set, measures) {
  pkgs = c("mlr3", learner$packages, measures$packages)
  require_namespaces(pkgs, sprintf("The following packages are required for measure %s: %%s", learner$id))

  result = vector("list", 7L)
  names(result) = c("model", "train_time", "train_log", "predicted", "test_time", "test_log", "performance")

  tmp = train_worker(task = task, learner = learner, train_set = train_set)
  result = insert(result, tmp)
  tmp = predict_worker(task = task, learner = learner, model = result$model, test_set = test_set)
  result = insert(result, tmp)
  tmp = score_worker(task = task, test_set = test_set, predicted = result$predicted, measures = measures)
  result = insert(result, tmp)

  return(result)
}
