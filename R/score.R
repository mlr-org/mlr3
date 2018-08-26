#' @export
score = function(experiment, measures = NULL) {
  assertExperiment(experiment)
  scoreExperiment(experiment$clone(), measures)
}

scoreExperiment = function(e, measures = NULL) {
  measures = asMeasures(measures, task = e$data$task)

  test.set = e$test.set
  pars = c(e$data[c("task", "predicted")], list(test.set = test.set, measures = measures))
  future = future::futureCall(scoreWorker, pars)
  e$data = insert(e$data, future::value(future))

  return(e)
}


scoreWorker = function(task, test.set, predicted, measures) {
  pkgs = c("mlr3", measures$packages)
  requireNamespaces(pkgs, sprintf("The following packages are required for measure %s: %%s", learner$id))

  truth = task$truth(test.set)[[1L]]
  if (length(truth) != length(predicted))
    stopf("Truth (length %i) and predicted (length %i) must have the same length", length(truth), length(predicted))
  if (length(predicted) == 0L)
    stop("Cannot estimate performance on zero-length predictions")

  performance = lapply(measures, function(m) m$fun(truth, predicted))
  names(performance) = ids(measures)
  return(list(performance = performance))
}
