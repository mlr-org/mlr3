#' @export
score = function(experiment, measures = NULL) {
  assert_experiment(experiment)
  score_experiment(experiment$clone(), measures)
}

score_experiment = function(e, measures = NULL) {
  measures = as_measures(measures, task = e$data$task)

  test_set = e$test_set
  pars = c(e$data[c("task", "predicted")], list(test_set = test_set, measures = measures))
  future = future::futureCall(score_worker, pars)
  e$data = insert(e$data, future::value(future))

  return(e)
}


score_worker = function(task, test_set, predicted, measures) {
  pkgs = c("mlr3", measures$packages)
  require_namespaces(pkgs, sprintf("The following packages are required for measure %s: %%s", learner$id))

  truth = task$truth(test_set)[[1L]]
  if (length(truth) != length(predicted))
    stopf("Truth (length %i) and predicted (length %i) must have the same length", length(truth), length(predicted))
  if (length(predicted) == 0L)
    stop("Cannot estimate performance on zero-length predictions")

  performance = lapply(measures, function(m) m$fun(truth, predicted))
  names(performance) = ids(measures)
  return(list(performance = performance))
}
