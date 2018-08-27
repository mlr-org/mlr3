#' @export
predict.Experiment = function(object, subset = NULL, newdata = NULL, ...) {
  if (...length() > 0L)
    stop("predict: dotargs currently unsupported!")
  predictExperiment(object$clone(), subset = subset, newdata = newdata)
}

predictExperiment = function(e, subset = NULL, newdata = NULL) {
  if (!is.null(subset) && !is.null(newdata))
    stopf("Arguments 'subset' and 'newdata' are mutually exclusive")

  if (is.null(newdata)) {
    test.set = e$data$task$row.ids(subset)
    e$data$resampling$instantiate(e$data$task, test.sets = list(test.set))
  } else {
    backend = BackendDataTable$new(data = newdata, primary_key = e$data$task$backend[[1L]]$primary_key)
    e$data$task = e$data$task$clone()$add_backend(backend)
    test.set = task$rows[role == "validation", "id"][[1L]]
    e$data$resampling$setTest(test.set)
  }

  future = future::futureCall(
    predictWorker,
    c(e$data[c("task", "learner", "model")], list(test.set = test.set))
  )
  e$data = insert(e$data, future::value(future))
  e$data = insert(e$data, list(performance = NULL))
  return(e)
}

predictWorker = function(task, learner, model, test.set) {
  pkgs = c("mlr3", learner$packages)
  requireNamespaces(pkgs, sprintf("The following packages are required for learner %s: %%s", learner$id))

  if (inherits(model, "dummy.model")) {
    learner = mlr.learners$get(sprintf("%s.dummy", learner$task.type))
  }
  pars = c(list(task = task, model = model, row.ids = test.set), learner$par.vals)
  fun = learner$predict
  now = proc.time()[[3L]]
  res = ecall(fun, pars)
  if (is.null(res$result)) {
    res$result = rep.int(task$default.prediction, length(test.set))
  }

  return(list(
    predicted = res$result,
    test.time = round(proc.time()[[3L]] - now, 8L),
    test.log = res$log,
    performance = NULL
  ))
}
