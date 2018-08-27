# do.call with evaluate
# this might get superseded by futures with output logging
ecall = function(fun, pars) {
  result = NULL
  log = evaluate::evaluate(
    "result <- do.call(fun, pars)",
    new_device = FALSE,
    include_timing = FALSE
  )

  if (length(log) == 1L) {
    log = data.table(msg = character(0L), type = character(0L))
  } else {
    log = log[-1L] # remove $src
    msg = vcapply(log, function(x) if (is.character(x)) x else x$message)
    type = vcapply(log, function(x) {
      if (is.character(x))
        return("output")
      if (inherits(x, "message") || inherits(x, "text"))
        return("message")
      if (inherits(x, "warning"))
        return("warning")
      if (inherits(x, "error"))
        return("error")
      stop("Unknown type while parsing log")
    })
    log = data.table(msg = msg, type = type)
  }
  log$type = factor(log$type, levels = c("output", "message", "warning", "error"))

  list(
    result = result,
    log = log
  )
}


train_worker = function(task, learner, train_set) {
  pkgs = c("mlr3", learner$packages)
  require_namespaces(pkgs, sprintf("The following packages are required for learner %s: %%s", learner$id))

  pars = c(list(task = task, row_ids = train_set), learner$par_vals)
  fun = learner$train
  now = proc.time()[[3L]]
  res = ecall(fun, pars)
  if (is.null(res$result)) {
    dummy = mlr_learners$get(sprintf("%s.dummy", learner$task_type))
    res$result = do.call(dummy$train, pars)
  }

  return(list(
    model = res$result,
    train_time = round(proc.time()[[3L]] - now, 8L),
    train_log = res$log
  ))
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

score_worker = function(task, test_set, predicted, measures) {
  pkgs = c("mlr3", measures$packages)
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

experiment_worker = function(task, learner, train_set, test_set, measures) {
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
