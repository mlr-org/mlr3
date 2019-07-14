learner_train = function(learner, task, row_ids = NULL, ctrl = mlr_control()) {
  # This wrapper calls learner$train, and additionally performs some basic
  # checks that the training was successful.
  # Exceptions here are possibly encapsulated, so that they get captured
  # and turned into log messages.
  wrapper = function(learner, task) {
    model = learner$train_internal(task = task)
    if (is.null(model)) {
      stopf("Learner '%s' returned NULL during train_internal()", learner$id)
    }
    model
  }

  task = assert_task(task)

  # subset to train set w/o cloning
  if (!is.null(row_ids)) {
    row_ids = assert_row_ids(row_ids)
    prev_use = task$row_roles$use
    on.exit({ task$row_roles$use = prev_use }, add = TRUE)
    task$row_roles$use = row_ids
  }

  # call wrapper with encapsulation
  enc = encapsulate(ctrl$encapsulate_train)
  result = enc(wrapper, list(learner = learner, task = task), learner$packages, seed = NA_integer_)

  learner$data$model = result$result
  learner$data$train_log = result$log
  learner$data$train_time = result$elapsed
  learner$data$predict_log = NULL
  learner$data$predict_time = NULL

  learner
}


learner_predict = function(learner, task, row_ids = NULL, ctrl = mlr_control()) {
  wrapper = function(task, learner) {
    if (is.null(learner$data$model)) {
      stopf("No trained model available")
    }

    result = learner$predict_internal(task = task)

    if (!inherits(result, "Prediction")) {
      stopf("Learner '%s' did not return a Prediction object, but instead: %s",
        learner$id, as_short_string(result))
    }

    unsupported = setdiff(names(result$data), c("row_ids", "truth", learner$predict_types))
    if (length(unsupported)) {
      stopf("Learner '%s' returned result for unsupported predict type '%s'", learner$id, head(unsupported, 1L))
    }

    return(result)
  }

  task = assert_task(task)

  if (!is.null(row_ids)) {
    row_ids = assert_row_ids(row_ids)
    prev_use = task$row_roles$use
    on.exit({ task$row_roles$use = prev_use }, add = TRUE)
    task$row_roles$use = row_ids
  }

  # call predict with encapsulation
  enc = encapsulate(ctrl$encapsulate_predict)
  result = enc(wrapper, list(task = task, learner = learner), learner$packages, seed = NA_integer_)

  learner$data$predict_log = result$log
  learner$data$predict_time = result$elapsed
  return(result$result)
}


workhorse = function(iteration, task, learner, resampling, ctrl = mlr_control(), remote = FALSE) {
  if (remote) {
    lg$set_threshold(ctrl$log_threshold)
  }
  lg$info("Applying learner '%s' on task '%s' (iter %i/%i)", learner$id, task$id, iteration, resampling$iters)
  train_set = resampling$train_set(iteration)
  test_set = resampling$test_set(iteration)

  learner = learner_train(learner$clone(), task, train_set, ctrl)
  prediction = learner_predict(learner, task, test_set, ctrl)

  if (!ctrl$store_models) {
    learner$data$model = NULL
  }

  list(learner_data = learner$data, prediction = prediction)
}

# called on the master, re-constructs objects from return value of
# the workhorse function
reassemble = function(result, learner) {
  learner = learner$clone()
  learner$data = result$learner_data
  list(learner = list(learner), prediction = list(result$prediction))
}
