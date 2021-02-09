learner_train = function(learner, task, row_ids = NULL) {
  # This wrapper calls learner$train, and additionally performs some basic
  # checks that the training was successful.
  # Exceptions here are possibly encapsulated, so that they get captured
  # and turned into log messages.
  train_wrapper = function(learner, task) {
    if (task$nrow == 0L) {
      stopf("Cannot fit Learner '%s' on task '%s': No observations", learner$id, task$id)
    }

    model = get_private(learner)$.train(task)

    if (is.null(model)) {
      stopf("Learner '%s' on task '%s' returned NULL during internal train()", learner$id, task$id)
    }

    model
  }


  assert_task(task)
  assert_learner(learner)
  assert_learnable(task, learner)

  # subset to train set w/o cloning
  if (!is.null(row_ids)) {
    lg$debug("Subsetting task '%s' to %i rows",
      task$id, length(row_ids), task = task$clone(), row_ids = row_ids)

    prev_use = task$row_roles$use
    on.exit({
      task$row_roles$use = prev_use
    }, add = TRUE)
    task$row_roles$use = row_ids
  } else {
    lg$debug("Skip subsetting of task '%s'", task$id)
  }

  learner$state = list()

  lg$debug("Calling train method of Learner '%s' on task '%s' with %i observations",
    learner$id, task$id, task$nrow, learner = learner$clone())

  # call train_wrapper with encapsulation
  result = encapsulate(learner$encapsulate["train"],
    .f = train_wrapper,
    .args = list(learner = learner, task = task),
    .pkgs = learner$packages,
    .seed = NA_integer_,
    .timeout = learner$timeout["train"]
  )

  learner$state = insert_named(learner$state, list(
    model = result$result,
    log = append_log(NULL, "train", result$log$class, result$log$msg),
    train_time = result$elapsed
  ))

  if (is.null(result$result)) {
    lg$debug("Learner '%s' on task '%s' failed to fit a model",
      learner$id, task$id, learner = learner$clone(), messages = result$log$msg)
  } else {
    lg$debug("Learner '%s' on task '%s' succeeded to fit a model",
      learner$id, task$id, learner = learner$clone(), result = result$result, messages = result$log$msg)
  }

  # fit fallback learner
  fb = learner$fallback
  if (!is.null(fb)) {
    lg$debug("Calling train method of fallback '%s' on task '%s' with %i observations",
      fb$id, task$id, task$nrow, learner = fb$clone())

    fb = assert_learner(as_learner(fb))
    require_namespaces(fb$packages)
    fb$train(task)
    learner$state$fallback_state = fb$state

    lg$debug("Fitted fallback learner '%s'",
      fb$id, learner = fb$clone())
  }

  learner
}


learner_predict = function(learner, task, row_ids = NULL) {
  # This wrapper calls learner$predict, and additionally performs some basic
  # checks that the prediction was successful.
  # Exceptions here are possibly encapsulated, so that they get captured and turned into log messages.
  predict_wrapper = function(task, learner) {
    if (is.null(learner$state$model)) {
      stopf("No trained model available for learner '%s' on task '%s'", learner$id, task$id)
    }

    if (exists("predict_internal", envir = learner, inherits = FALSE)) {
      .Deprecated(msg = "Use private method '.predict()' instead of public method 'predict_internal()'")
      result = learner$predict_internal(task)
    } else {
      result = get_private(learner)$.predict(task)
    }

    as_prediction_data(result, task = task, check = TRUE)
  }

  assert_task(task)
  assert_learner(learner)
  assert_learnable(task, learner)

  # subset to test set w/o cloning
  if (!is.null(row_ids)) {
    lg$debug("Subsetting task '%s' to %i rows",
      task$id, length(row_ids), task = task$clone(), row_ids = row_ids)

    prev_use = task$row_roles$use
    on.exit({
      task$row_roles$use = prev_use
    }, add = TRUE)
    task$row_roles$use = row_ids
  } else {
    lg$debug("Skip subsetting of task '%s'", task$id)
  }

  if (task$nrow == 0L) {
    # return an empty prediction object, #421
    lg$debug("No observations in task, returning empty prediction data", task = task)
    learner$state$log = append_log(learner$state$log, "predict", "output", "No data to predict on")
    return(as_prediction_data(named_list(), task = task, row_ids = integer(), check = TRUE))
  }

  if (is.null(learner$state$model)) {
    lg$debug("Learner '%s' has no model stored",
      learner$id, learner = learner$clone())

    prediction = NULL
    learner$state$predict_time = NA_real_
  } else {
    # call predict with encapsulation
    lg$debug("Calling predict method of Learner '%s' on task '%s' with %i observations",
      learner$id, task$id, task$nrow, learner = learner$clone())

    result = encapsulate(
      learner$encapsulate["predict"],
      .f = predict_wrapper,
      .args = list(task = task, learner = learner),
      .pkgs = learner$packages,
      .seed = NA_integer_,
      .timeout = learner$timeout["predict"]
    )

    prediction = result$result
    learner$state$log = append_log(learner$state$log, "predict", result$log$class, result$log$msg)
    learner$state$predict_time = result$elapsed

    lg$debug("Learner '%s' returned an object of class '%s'",
      learner$id, class(prediction)[1L], learner = learner$clone(), prediction = prediction, messages = result$log$msg)
  }


  fb = learner$fallback
  if (!is.null(fb)) {
    predict_fb = function(row_ids) {
      fb = assert_learner(as_learner(fb))
      fb$predict_type = learner$predict_type
      fb$state = learner$state$fallback_state
      as_prediction_data(fb$predict(task, row_ids), task, row_ids, check = TRUE)
    }


    if (is.null(prediction)) {
      lg$debug("Creating new Prediction using fallback '%s'",
        fb$id, learner = fb$clone())

      learner$state$log = append_log(learner$state$log, "predict", "output", "Using fallback learner for predictions")
      prediction = predict_fb(task$row_ids)
    } else {
      miss_ids = is_missing_prediction_data(prediction)

      lg$debug("Imputing %i%i predictions using fallback '%s'",
        length(miss_ids), length(prediction$row_ids), fb$id,  learner = fb$clone())

      if (length(miss_ids)) {
        learner$state$log = append_log(learner$state$log, "predict", "output", "Using fallback learner to impute predictions")

        prediction = c(prediction, predict_fb(miss_ids), keep_duplicates = FALSE)
      }
    }
  }

  return(prediction)
}


workhorse = function(iteration, task, learner, resampling, lgr_threshold = NULL, store_models = FALSE, pb = NULL) {
  if (!is.null(pb)) {
    pb(sprintf("%s|%s|i:%i", task$id, learner$id, iteration))
  }

  if (!is.null(lgr_threshold)) {
    lg$set_threshold(lgr_threshold)
  }

  lg$info("Applying learner '%s' on task '%s' (iter %i/%i)",
    learner$id, task$id, iteration, resampling$iters)

  sets = list(
    train = resampling$train_set(iteration),
    test = resampling$test_set(iteration),
    validation = task$row_roles$validation
  )

  # train model
  learner = learner_train(learner$clone(), task, sets[["train"]])

  # predict for each set
  sets = sets[learner$predict_sets]
  sets = sets[lengths(sets) > 0L]
  pdatas = Map(function(set, row_ids) {
    lg$debug("Creating Prediction for predict set '%s'", set)
    learner_predict(learner, task, row_ids)
  }, set = names(sets), row_ids = sets)
  pdatas = discard(pdatas, is.null)

  if (!store_models) {
    lg$debug("Erasing stored model for learner '%s'", learner$id)
    learner$state$model = NULL
  }

  list(learner_state = learner$state, prediction = pdatas)
}

append_log = function(log = NULL, stage = NA_character_, class = NA_character_, msg = character()) {
  if (is.null(log)) {
    log = data.table(
      stage = factor(levels = c("train", "predict")),
      class = factor(levels = c("output", "warning", "error"), ordered = TRUE),
      msg = character()
    )
  }

  if (length(msg)) {
    log = rbindlist(list(log, data.table(stage = stage, class = class, msg = msg)), use.names = TRUE)
  }

  log
}
