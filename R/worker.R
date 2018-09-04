ecall = function(fun, pars) {
  result = do.call(fun, pars)
  log = data.table(msg = character(0L), type = character(0L))
  list(result = result, log = log)
}


train_worker = function(task, learner, train_set) {
  pkgs = c("mlr3", learner$packages)
  require_namespaces(pkgs, sprintf("The following packages are required for learner %s: %%s", learner$id))

  task = task$clone(deep = TRUE)$filter(train_set)
  pars = c(list(task = task), learner$par_vals)

  train_time = proc.time()[[3L]]
  res = ecall(learner$train, pars)
  train_time = round(proc.time()[[3L]] - train_time, 8L)

  learner$model = res$result

  return(list(
    learner = learner,
    train_time = train_time,
    train_log = res$log
  ))
}

predict_worker = function(task, learner, model, test_set) {
  pkgs = c("mlr3", learner$packages)
  require_namespaces(pkgs, sprintf("The following packages are required for learner %s: %%s", learner$id))

  task = task$clone(deep = TRUE)$filter(test_set)
  pars = c(list(task = task), learner$par_vals)
  test_time = proc.time()[[3L]]
  res = ecall(learner$predict, pars)
  test_time = round(proc.time()[[3L]] - test_time, 8L)

  return(list(
    predicted = res$result,
    test_time = test_time,
    test_log = res$log,
    performance = NULL
  ))
}

score_worker = function(task, test_set, predicted, measures) {
  measures = task$measures
  pkgs = c("mlr3", unlist(lapply(measures, "[[", "packages")))
  require_namespaces(pkgs, "The following packages are required for the measures: %s")

  truth = task$truth(test_set)[[1L]]
  if (length(truth) != length(predicted))
    stopf("Truth (length %i) and predicted (length %i) must have the same length", length(truth), length(predicted))
  if (length(predicted) == 0L)
    stop("Cannot estimate performance on zero-length predictions")

  performance = lapply(measures, function(m) m$fun(truth, predicted))
  names(performance) = ids(measures)
  return(list(performance = performance))
}

experiment_worker = function(task, learner, train_set, test_set) {
  result = vector("list", 7L)
  names(result) = c("learner", "train_time", "train_log", "predicted", "test_time", "test_log", "performance")

  tmp = train_worker(task = task, learner = learner, train_set = train_set)
  result = insert(result, tmp)
  tmp = predict_worker(task = task, learner = learner, model = result$model, test_set = test_set)
  result = insert(result, tmp)
  tmp = score_worker(task = task, test_set = test_set, predicted = result$predicted)
  result = insert(result, tmp)

  return(result)
}
