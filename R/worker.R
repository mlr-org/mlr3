train_worker = function(task, learner, train_set, ctrl, seed = NA_integer_) {

  # This wrapper calls learner$train, and additionally performs some basic
  # checks that the training was successful.
  # Exceptions here are possibly encapsulated, so that they get captured
  # and turned into log messages.
  wrapper = function(learner, task) {
    result = learner$train(task = task)
    if (!inherits(result, "Learner")) {
      stopf("Learner '%s' did not return a Learner during train()", learner$id)
    }
    if (is.null(learner$model)) {
      stopf("Learner '%s' did not store a model during train()", learner$id)
    }
    result
  }

  # subset to train set w/o cloning
  prev_use = task$row_roles$use
  task$row_roles$use = train_set
  on.exit({
    task$row_roles$use = prev_use
  }, add = TRUE)

  learner = learner$clone(deep = TRUE)

  lg$debug("train_worker: Learner '%s', task '%s' [%ix%i]", learner$id, task$id, task$nrow, task$ncol)

  # call wrapper with encapsulation
  enc = encapsulate(ctrl$encapsulate_train)
  result = (enc(wrapper, list(learner = learner, task = task), learner$packages, seed = seed))
  names(result) = c("learner", "train_log", "train_time")

  # result is list(learner, train_log, train_time)
  return(result)
}


predict_worker = function(task, learner, test_set, ctrl, seed = NA_integer_) {

  # This wrapper calls learner$predict, and additionally performs some basic
  # checks that the prediction was successful.
  # Exceptions here are possibly encapsulated, so that they get captured
  # and turned into log messages.
  wrapper = function(task, learner) {

    if (is.null(learner$model)) {
      stopf("No trained model available")
    }

    result = learner$predict(task = task)

    if (is.null(result)) {
      stopf("Learner '%s' returned NULL during predict()", learner$id)
    }

    if (!inherits(result, "PredictionData")) {
      stopf("Learner '%s' returned '%s' during predict(), but needs to return a PredictionData object as returned by ?as_prediction",
        learner$id, as_short_string(result))

    }

    unsupported = setdiff(names(result), c("row_ids", learner$predict_types))
    if (length(unsupported)) {
      stopf("Learner '%s' returned result for unsupported predict type '%s'", learner$id, head(unsupported, 1L))
    }

    return(result)
  }

  # subset to test set w/o cloning
  prev_use = task$row_roles$use
  task$row_roles$use = test_set
  on.exit({
    task$row_roles$use = prev_use
  }, add = TRUE)

  # call predict with encapsulation
  enc = encapsulate(ctrl$encapsulate_predict)
  result = enc(wrapper, list(task = task, learner = learner), learner$packages, seed = seed)
  names(result) = c("predicted", "predict_log", "predict_time")

  # update the model if necessary
  # if ("updates_model" %in% learner$properties) {
  #   result$learner = learner
  # }

  # result is list(predicted, predict_log, predict_time)
  return(result)
}


score_worker = function(e, ctrl) {

  data = e$data
  measures = data$measures
  pkgs = unique(unlist(map(measures, "packages")))

  lg$debug("score_worker: Learner '%s' on task '%s' [%ix%i]", data$learner$id, data$task$id, data$task$nrow, data$task$ncol)


  score_one = function(m) {
    m$calculate(experiment = e, prediction = prediction)
  }

  score = function() {
    set_names(lapply(measures, score_one), ids(measures))
  }

  # build the prediction object once
  prediction = e$prediction

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
    lg$set_threshold(ctrl$log_threshold)
  }

  # Create a new experiment
  # Results will be inserted into e$data in a piecemeal fashion
  e = as_experiment(task = task, learner = learner, resampling = resampling, iteration = iteration, measures = measures)

  lg$info("Running learner '%s' on task '%s' (iteration %i/%i)'", learner$id, task$id, iteration, resampling$iters)

  train_set = resampling$train_set(iteration)
  tmp = train_worker(task, learner, train_set, ctrl)
  e$data = insert_named(e$data, tmp)

  test_set = resampling$test_set(iteration)
  tmp = predict_worker(task, e$data$learner, test_set, ctrl)
  e$data = insert_named(e$data, tmp)

  tmp = score_worker(e, ctrl)
  e$data = insert_named(e$data, tmp)

  if (!ctrl$store_prediction) {
    e$data["predicted"] = list(NULL)
  }

  if (!ctrl$store_model) {
    e$data$learner$model = NULL
  }

  # Remove slots which are already known by the calling function and return data slot
  remove_named(e$data, c("task", "resampling", "measures"))
}
