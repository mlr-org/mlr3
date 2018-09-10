ecall = function(fun, pars) {
  result = do.call(fun, pars)
  log = data.table(msg = character(0L), type = character(0L))
  list(result = result, log = log)
}

train_worker = function(e, ctrl) {
  learner = e$data$learner
  pkgs = c("mlr3", learner$packages)
  require_namespaces(pkgs, sprintf("The following packages are required for learner %s: %%s", learner$id))

  task = e$data$task$clone(deep = TRUE)$filter(e$train_set)
  pars = c(list(task = task), learner$par_vals)

  if (ctrl$verbose)
    message(sprintf("Training learner '%s' on task '%s' ...", learner$id, task$id))
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

predict_worker = function(e, ctrl) {
  learner = e$data$learner
  pkgs = c("mlr3", learner$packages)
  require_namespaces(pkgs, sprintf("The following packages are required for learner %s: %%s", learner$id))

  task = e$data$task$clone(deep = TRUE)$filter(e$test_set)
  pars = c(list(task = task), learner$par_vals)

  if (ctrl$verbose)
    message(sprintf("Predicting model of learner '%s' on task '%s' ...", learner$id, task$id))
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

score_worker = function(e, ctrl) {
  task = e$data$task
  measures = task$measures
  pkgs = c("mlr3", unlist(lapply(measures, "[[", "packages")))
  require_namespaces(pkgs, "The following packages are required for the measures: %s")

  if (ctrl$verbose)
    message(sprintf("Scoring predictions of learner '%s' on task '%s' ...", e$data$learner$id, e$data$task$id))
  performance = lapply(measures, function(m) m$calculate(e))
  names(performance) = ids(measures)
  return(list(performance = performance))
}

experiment_worker = function(iteration, task, learner, resampling, ctrl) {
  e = Experiment$new(task, learner)
  e$data = insert(e$data, list(resampling = resampling, iteration = iteration))

  if (ctrl$verbose) {
    message(sprintf("Running learner '%s' on task '%s (iteration %i/%i)' ...", learner$id, task$id, iteration, resampling$iters))
    ctrl$verbose = FALSE
  }

  tmp = train_worker(e, ctrl)
  e$data = insert(e$data, tmp)

  tmp = predict_worker(e, ctrl)
  e$data = insert(e$data, tmp)

  tmp = score_worker(e, ctrl)
  e$data = insert(e$data, tmp)

  remove(e$data, c("task", "resampling"))
}
