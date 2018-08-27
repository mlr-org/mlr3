#' @export
predict.Experiment = function(object, subset = NULL, newdata = NULL, ...) {
  if (...length() > 0L)
    stop("predict: dotargs currently unsupported!")
  predict_experiment(object$clone(), subset = subset, newdata = newdata)
}

predict_experiment = function(e, subset = NULL, newdata = NULL) {
  if (!is.null(subset) && !is.null(newdata))
    stopf("Arguments 'subset' and 'newdata' are mutually exclusive")

  if (is.null(newdata)) {
    test_set = e$data$task$row_ids(subset)
    e$data$resampling$instantiate(e$data$task, test_sets = list(test_set))
  } else {
    backend = BackendDataTable$new(data = newdata, primary_key = e$data$task$backend[[1L]]$primary_key)
    e$data$task = e$data$task$clone()$add_backend(backend)
    test_set = task$rows[role == "validation", "id"][[1L]]
    e$data$resampling$setTest(test_set)
  }

  future = future::futureCall(
    predict_worker,
    c(e$data[c("task", "learner", "model")], list(test_set = test_set))
  )
  e$data = insert(e$data, future::value(future))
  e$data = insert(e$data, list(performance = NULL))
  return(e)
}

predict_worker = function(task, learner, model, test_set) {
  pkgs = c("mlr3", learner$packages)
  require_namespaces(pkgs, sprintf("The following packages are required for learner %s: %%s", learner$id))

  if (inherits(model, "dummy.model")) {
    learner = mlr_learners$get(sprintf("%s.dummy", learner$task_type))
  }
  pars = c(list(task = task, model = model, row_ids = test_set), learner$par_vals)
  fun = learner$predict
  now = proc.time()[[3L]]
  res = ecall(fun, pars)
  if (is.null(res$result)) {
    res$result = rep.int(task$default_prediction, length(test_set))
  }

  return(list(
    predicted = res$result,
    test_time = round(proc.time()[[3L]] - now, 8L),
    test_log = res$log,
    performance = NULL
  ))
}
