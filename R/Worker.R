runExperiment = function(task, learner, train.set, test.set, measures) {
  pkgs = c("mlr3", learner$packages, measures$packages)
  requireNamespaces(pkgs, sprintf("The following packages are required for measure %s: %%s", learner$id))

  result = vector("list", 7L)
  names(result) = c("model", "train.time", "train.log", "predicted", "test.time", "test.log", "performance")

  tmp = trainWorker(task = task, learner = learner, train.set = train.set)
  result = insert(result, tmp)
  tmp = predictWorker(task = task, learner = learner, model = result$model, test.set = test.set)
  result = insert(result, tmp)
  tmp = scoreWorker(task = task, test.set = test.set, predicted = result$predicted, measures = measures)
  result = insert(result, tmp)

  return(result)
}
