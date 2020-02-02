# This wrapper calls learner$train, and additionally performs some basic
# checks that the training was successful.
# Exceptions here are possibly encapsulated, so that they get captured
# and turned into log messages.
train_wrapper = function(learner, task) {
  model = learner$train_internal(task = task)

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

  result = learner$predict_internal(task = task)

  if (!inherits(result, "Prediction")) {
    stopf("Learner '%s' on task '%s' did not return a Prediction object, but instead: %s",
      learner$id, task$id, as_short_string(result))
  }

  return(result)
}


learner_train = function(learner, task, row_ids = NULL) {
  assert_task(task)

  # subset to train set w/o cloning
  if (!is.null(row_ids)) {
    prev_use = task$row_roles$use
    on.exit({
      task$row_roles$use = prev_use
    }, add = TRUE)
    task$row_roles$use = row_ids
  }

  learner$state = list()

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

  learner$state = insert_named(learner$state, list(
    model = result$result,
    log = append_log(NULL, "train", result$log$class, result$log$msg),
    train_time = result$elapsed
  ))

  # fit fallback learner
  fb = learner$fallback
  if (!is.null(fb)) {
    fb = assert_learner(as_learner(fb))
    require_namespaces(fb$packages)
    fb$train(task)
    learner$state$fallback_state = fb$state
  }

  learner
}


learner_predict = function(learner, task, row_ids = NULL) {
  assert_task(task)

  # subset to test set w/o cloning
  if (!is.null(row_ids)) {
    prev_use = task$row_roles$use
    on.exit({
      task$row_roles$use = prev_use
    }, add = TRUE)
    task$row_roles$use = row_ids
  }

  if (task$nrow == 0L) {
    # return an empty prediction object, #421
    learner$state$log = append_log(learner$state$log, "predict", "output", "No data to predict on")
    tt = task$task_type
    f = mlr_reflections$task_types[list(tt), "prediction", with = FALSE][[1L]]
    return(get(f)$new(task = task))
  }

  if (is.null(learner$model)) {
    prediction = NULL
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
    learner$state$log = append_log(learner$state$log, "predict", result$log$class, result$log$msg)
    learner$state$predict_time = result$elapsed
  }


  fb = learner$fallback
  if (!is.null(fb)) {
    predict_fb = function(row_ids) {
      fb = assert_learner(as_learner(fb))
      fb$predict_type = learner$predict_type
      fb$state = learner$state$fallback_state
      fb$predict(task, row_ids)
    }

    if (is.null(prediction)) {
      learner$state$log = append_log(learner$state$log, "predict", "output", "Using fallback learner for predictions")
      prediction = predict_fb(task$row_ids)
    } else {
      miss_ids = prediction$missing
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
  lg$info("Applying learner '%s' on task '%s' (iter %i/%i)", learner$id, task$id, iteration, resampling$iters)

  sets = list(train = resampling$train_set(iteration), test = resampling$test_set(iteration))
  learner = learner_train(learner$clone(), task, sets[["train"]])

  prediction = lapply(sets[learner$predict_sets], function(set) {
    learner_predict(learner, task, set)
  })
  names(prediction) = learner$predict_sets
  prediction = prediction[!vapply(prediction, is.null, NA)]

  if (!store_models) {
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
