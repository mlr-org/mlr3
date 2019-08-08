# This wrapper calls learner$train, and additionally performs some basic
# checks that the training was successful.
# Exceptions here are possibly encapsulated, so that they get captured
# and turned into log messages.
train_wrapper = function(learner, task) {
  tryCatch(
    { model = learner$train_internal(task = task) },
    error = function(e) {
      e$message = sprintf("Learner '%s' on task '%s' failed to fit: %s", learner$id, task$id, e$message)
      stop(e)
    }
  )

  if (is.null(model)) {
    stopf("Learner '%s' on task '%s' returned NULL during train_internal()", learner$id, task$id)
  }

  model
}


# This wrapper calls learner$predict, and additionally performs some basic
# checks that the prediction was successful.
# Exceptions here are possibly encapsulated, so that they get captured and turned into log messages.
predict_wrapper = function(task, learner) {
  if (is.null(learner$state$model)) {
    stopf("No trained model available for learner '%s' on task '%s'", learner$id, task$id)
  }

  result = tryCatch(
    learner$predict_internal(task = task),
    error = function(e) {
      e$message = sprintf("Learner '%s' on task '%s' failed to predict: %s", learner$id, task$id, e$message)
      stop(e)
    }
  )

  if (!inherits(result, "Prediction")) {
    stopf("Learner '%s' on task '%s' did not return a Prediction object, but instead: %s",
      learner$id, task$id, as_short_string(result))
  }

  unsupported = setdiff(names(result$data), c("row_ids", "truth", learner$predict_types))
  if (length(unsupported)) {
    stopf("Learner '%s' on task '%s' returned result for unsupported predict type '%s'", learner$id, task$id, head(unsupported, 1L))
  }

  return(result)
}


learner_train = function(learner, task, row_ids = NULL, ctrl = mlr_control()) {
  task = assert_task(task)

  # subset to train set w/o cloning
  if (!is.null(row_ids)) {
    row_ids = assert_row_ids(row_ids)
    prev_use = task$row_roles$use
    on.exit({ task$row_roles$use = prev_use }, add = TRUE)
    task$row_roles$use = row_ids
  }

  # call train_wrapper with encapsulation
  result = encapsulate(learner$encapsulate["train"],
    .f = train_wrapper,
    .args = list(learner = learner, task = task),
    .pkgs = learner$packages,
    .seed = NA_integer_
  )

  if (is.null(result$result)) {
    lg$debug("Learner '%s' on task '%s' did not fit a model", learner$id, task$id, learner = learner$clone(), task = task$clone())
  }

  learner$state = list(
    model = result$result,
    train_log = result$log,
    train_time = result$elapsed,
    predict_log = NULL,
    predict_time = NULL
  )

  # fit fallback learner
  fb = learner$fallback
  if (!is.null(fb)) {
    fb = assert_learner(fb)
    require_namespaces(fb$packages)
    fb$train(task)
    learner$state$fallback_state = fb$state
  }

  learner
}


learner_predict = function(learner, task, row_ids = NULL, ctrl = mlr_control()) {
  task = assert_task(task)

  # subset to test set w/o cloning
  if (!is.null(row_ids)) {
    row_ids = assert_row_ids(row_ids)
    prev_use = task$row_roles$use
    on.exit({ task$row_roles$use = prev_use }, add = TRUE)
    task$row_roles$use = row_ids
  }

  if (is.null(learner$model)) {
    prediction = NULL
    learner$state$predict_log = data.table(
      class = factor("warning", levels = c("output", "warning", "error"), ordered = TRUE),
      msg = "No model trained"
    )
    learner$state$predict_time = NA_real_
  } else {
    # call predict with encapsulation
    result = encapsulate(
      learner$encapsulate["predict"],
      .f = predict_wrapper,
      .args = list(task = task, learner = learner),
      .pkgs = learner$packages,
      .seed = NA_integer_
    )

    prediction = result$result
    learner$state$predict_log = result$log
    learner$state$predict_time = result$elapsed
  }

  predict_fb = function(row_ids) {
    fb = assert_learner(fb)
    fb$predict_type = learner$predict_type
    fb$state = learner$state$fallback_state
    fb$predict(task, row_ids)
  }

  fb = learner$fallback
  if (!is.null(fb)) {
    if (is.null(prediction)) {
      prediction = predict_fb(task$row_ids)
    } else {
      miss_ids = prediction$missing
      if (length(miss_ids)) {
        prediction = c(prediction, predict_fb(miss_ids), keep_duplicates = FALSE)
      }
    }
  }

  return(prediction)
}


workhorse = function(iteration, task, learner, resampling, ctrl = mlr_control(), lgr_threshold = NULL) {
  if (!is.null(lgr_threshold)) {
     lg$set_threshold(lgr_threshold)
  }
  lg$info("Applying learner '%s' on task '%s' (iter %i/%i)", learner$id, task$id, iteration, resampling$iters)
  train_set = resampling$train_set(iteration)
  test_set = resampling$test_set(iteration)

  learner = learner_train(learner$clone(), task, train_set, ctrl)
  prediction = learner_predict(learner, task, test_set, ctrl)

  if (!ctrl$store_models) {
    learner$state$model = NULL
  }

  list(learner_state = learner$state, prediction = prediction)
}

# called on the master, re-constructs objects from return value of
# the workhorse function
reassemble = function(result, learner) {
  learner = learner$clone()
  learner$state = result$learner_state
  list(learner = list(learner), prediction = list(result$prediction))
}
