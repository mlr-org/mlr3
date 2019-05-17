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

    if (!inherits(result, "Learner")) {
      abort("Learner '%s' returned '%s' during train(), but needs to return a Learner", learner$id, as_short_string(result))
    }

    if (is.null(result$model)) {
      abort("Learner '%s' did not store a model during train", learner$id)
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
    c("learner", "train_log", "train_time"))

  # Restore the learner to the untrained learner otherwise
  if (!is.null(result$train_log)) {
    errors = result$train_log[get("class") == "error", .N]
    if (errors > 0L) {
      result$learner = learner$clone(deep = TRUE)
    }
  }

  # if there is a fallback learner defined, also fit fallback learner
  fb = learner$fallback
  if (!is.null(fb)) {
    log_debug("train_worker: Training fallback learner '%s' on task '%s'", fb$id, task$id, namespace = "mlr3")
    require_namespaces(fb$packages, sprintf("The following packages are required for fallback learner %s: %%s", fb$id))

    ok = try(fb$train(task))
    if (inherits(ok, "try-error")) {
      abort("Fallback learner '%s' failed during train() with error: %s", fb$id, as.character(ok))
    }
    if (!inherits(ok, "Learner")) {
      abort("Fallback-Learner '%s' returned '%s' during train(), but needs to return a Learner",
        fb$id, as_short_string(result))
    }
    if (is.null(ok$model)) {
      abort("Fallback learner '%s' did not store a model during train", fb$id)
    }

    result$learner$fallback = ok
  }

  # result is list(learner, train_log, train_time)
  return(result)
}


predict_worker = function(task, learner, test_set, ctrl, seed = NA_integer_) {

  abort = function(e, ...) {
    msg = sprintf(as.character(e), ...)
    stop(errorCondition(msg, learner = learner, task = task, class = "predictError"))
  }

  # This wrapper calls learner$predict, and additionally performs some basic
  # checks that the prediction was successful.
  # Exceptions here are possibly encapsulated, so that they get captured
  # and turned into log messages.
  wrapper = function(learner, task) {
    if (is.null(learner$model)) {
      abort("No trained model available")
    }

    result = tryCatch(learner$predict(task), error = abort)

    if (is.null(result)) {
      abort("Learner '%s' returned NULL during predict()", learner$id)
    }

    if (!inherits(result, "Prediction")) {
      abort("Learner '%s' returned '%s' during predict(), but needs to return a Prediction object", learner$id, as_short_string(result))
    }

    return(result)
  }

  task = task$clone(deep = TRUE)$filter(test_set)
  if (is.null(learner$model)) {
    if (!is.null(learner$fallback)) {
      learner = learner$fallback
    } else {
      abort("No model fitted during train()")
    }
  }

  # call predict with encapsulation
  enc = encapsulate(ctrl$encapsulate_predict)
  res = set_names(enc(wrapper, list(learner = learner, task = task), learner$packages, seed = seed),
    c("prediction", "predict_log", "predict_time"))

  if (!is.null(res$predict_log) && res$predict_log[get("class") == "error", .N] > 0L) {
    fb = learner$fallback
    if (!is.null(fb)) {
      log_debug("predict_worker: Predicting fallback learner '%s' on task '%s'", fb$id, task$id, namespace = "mlr3")
      require_namespaces(fb$packages, sprintf("The following packages are required for fallback learner %s: %%s", fb$id))

      ok = try(fb$predict(task))
      if (inherits(ok, "try-error")) {
        abort("Fallback learner '%s' failed during predict() with error: %s", fb$id, as.character(ok))
      }
      if (!inherits(ok, "Prediction")) {
        abort("Fallback-Learner '%s' returned '%s' during predict(), but needs to return a Prediction",
          fb$id, as_short_string(res))
      }

      res$prediction = ok
    }
  }

  # result is list(prediction, predict_log, predict_time)
  return(res)
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
  res = enc(score, list(), pkgs, seed = e$seeds[["score"]])

  return(list(performance = res$result, score_time = res$elapsed))
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

  tmp = train_worker(e$task, e$learner, e$train_set, ctrl)
  e$data = insert_named(e$data, tmp)

  tmp = predict_worker(e$task, e$learner, e$test_set, ctrl)
  e$data = insert_named(e$data, tmp)

  tmp = score_worker(e, ctrl)
  e$data = insert_named(e$data, tmp)

  if (!ctrl$store_prediction) {
    e$data["prediction"] = list(NULL)
  }

  if (!ctrl$store_model) {
    e$data$learner$model = NULL
  }

  # Remove slots which are already known by the calling function and return data slot
  remove_named(e$data, c("task", "resampling", "measures"))
}
