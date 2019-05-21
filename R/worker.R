train_worker = function(task, learner, train_set, ctrl, seed = NA_integer_) {

  abort = function(e, ...) {
    msg = sprintf(as.character(e), ...)
    stop(errorCondition(msg, learner = learner, task = task, class = "trainError"))
  }

  # This wrapper calls learner$train, and additionally performs some basic
  # checks that the training was successful.
  # Exceptions here are possibly encapsulated, so that they get captured
  # and turned into log messages.
  wrapper = function(learner, task) {
    result = tryCatch(learner$train(task), error = abort)
    if (is.null(result)) {
      abort("Learner '%s' returned NULL during train", learner$id)
    }
    result
  }

  # we are going to change learner$model, so make sure we clone it first
  learner = learner$clone(deep = TRUE)

  # subset task
  task = task$clone(deep = TRUE)$filter(train_set)

  log_debug("train_worker: Learner '%s', task '%s' [%ix%i]", learner$id, task$id, task$nrow, task$ncol, namespace = "mlr3")

  # call wrapper with encapsulation
  enc = encapsulate(ctrl$encapsulate_train)
  result = set_names(enc(wrapper, list(learner = learner, task = task), learner$packages, seed = seed),
    c("model", "train_log", "train_time"))

  # result is list(model, train_log, train_time)
  return(result)
}


predict_worker = function(task, learner, model, test_set, ctrl, seed = NA_integer_) {

  abort = function(e, ...) {
    msg = sprintf(as.character(e), ...)
    stop(errorCondition(msg, learner = learner, task = task, class = "predictError"))
  }

  # This wrapper calls learner$predict, and additionally performs some basic
  # checks that the prediction was successful.
  # Exceptions here are possibly encapsulated, so that they get captured
  # and turned into log messages.
  wrapper = function(task, learner, model) {
    if (is.null(model)) {
      abort("No trained model available")
    }

    result = tryCatch(learner$predict(task), error = abort)

    if (is.null(result)) {
      abort("Learner '%s' returned NULL during predict()", learner$id)
    }

    if (!testList(result, names = "unique")) {
      abort("Learner '%s' returned '%s' during predict(), but needs to return a named list",
        learner$id, as_short_string(result))
    }

    i = wf(names(result) %nin% learner$predict_types)
    if (length(i)) {
      abort("Learner '%s' returned result for unsupported predict type '%s'", learner$id, names(result)[i])
    }

    return(result)
  }

  # subset to test set
  task = task$clone(deep = TRUE)$filter(test_set)

  # call predict with encapsulation
  enc = encapsulate(ctrl$encapsulate_predict)
  result = set_names(enc(wrapper, list(task = task, learner = learner, model = model), learner$packages, seed = seed),
    c("prediction", "predict_log", "predict_time"))

  # check and convert prediction of the learner
  result$prediction = convert_prediction(task, result$prediction)

  # store updated model if required
  if ("updates_model" %in% learner$properties)
    result$model = learner$model

  # result is list(prediction, predict_log, predict_time)
  return(result)
}


score_worker = function(e, ctrl) {

  data = e$data
  measures = data$measures
  pkgs = unique(unlist(map(measures, "packages")))

  log_debug("score_worker: Learner '%s' on task '%s' [%ix%i]", data$learner$id, data$task$id, data$task$nrow, data$task$ncol, namespace = "mlr3")


  score_one = function(m) {
    abort = function(e, ...) {
      msg = sprintf(as.character(e), ...)
      stop(errorCondition(msg, experiment = e, measure = m, class = "scoreError"))
    }
    tryCatch(m$calculate(experiment = e), error = abort)
  }
  score = function() {
    set_names(lapply(measures, score_one), ids(measures))
  }

  # call m$score with local encapsulation
  enc = encapsulate("none")
  result = enc(score, list(), pkgs, seed = e$seeds[["score"]])

  return(list(performance = result$result, score_time = result$elapsed))
}


# this gem here is parallelized.
# thus, we want the in- and output to be minimal
experiment_worker = function(iteration, task, learner, resampling, measures, ctrl, remote = FALSE) {

  if (remote) {
    # restore the state of the master session
    # currently, this only affects logging as we do not use any global options
    logger::log_threshold(ctrl$log_threshold, namespace = "mlr3")
  }

  # Create a new experiment
  # Results will be inserted into e$data in a piecemeal fashion
  e = as_experiment(task = task, learner = learner, resampling = resampling, iteration = iteration, measures = measures)

  log_info("Running learner '%s' on task '%s' (iteration %i/%i)'", learner$id, task$id, iteration, resampling$iters, namespace = "mlr3")

  train_set = resampling$train_set(iteration)
  tmp = train_worker(task, learner, train_set, ctrl)
  e$data = insert_named(e$data, tmp)

  test_set = resampling$test_set(iteration)
  tmp = predict_worker(task, e$learner, e$data$model, test_set, ctrl)
  e$data = insert_named(e$data, tmp)

  tmp = score_worker(e, ctrl)
  e$data = insert_named(e$data, tmp)

  if (!ctrl$store_prediction) {
    e$data["prediction"] = list(NULL)
  }

  if (!ctrl$store_model) {
    e$data[c("model")] = list(NULL)
  }

  # Remove slots which are already known by the calling function and return data slot
  remove_named(e$data, c("task", "learner", "resampling", "measures"))
}
