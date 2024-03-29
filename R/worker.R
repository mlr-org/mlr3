learner_train = function(learner, task, train_row_ids = NULL, test_row_ids = NULL, mode = "train") {
  # This wrapper calls learner$train, and additionally performs some basic
  # checks that the training was successful.
  # Exceptions here are possibly encapsulated, so that they get captured
  # and turned into log messages.
  train_wrapper = function(learner, task) {
    if (task$nrow == 0L) {
      stopf("Cannot %s Learner '%s' on task '%s': No observations", mode, learner$id, task$id)
    }

    model = if (mode == "train") {
      get_private(learner)$.train(task)
    } else if (mode == "hotstart") {
      get_private(learner)$.hotstart(task)
    }

    if (is.null(model)) {
      stopf("Learner '%s' on task '%s' returned NULL during internal %s()", learner$id, task$id, mode)
    }

    model
  }

  assert_choice(mode, c("train", "hotstart"))
  assert_task(task)
  assert_learner(learner)

  # ensure that required packages are installed
  require_namespaces(learner$packages)

  # subset to train set w/o cloning
  if (!is.null(train_row_ids)) {
    lg$debug("Subsetting task '%s' to %i rows",
      task$id, length(train_row_ids), task = task$clone(), row_ids = train_row_ids)

    prev_use = task$row_roles$use
    on.exit({
      task$row_roles$use = prev_use
    }, add = TRUE)
    task$row_roles$use = train_row_ids
  } else {
    lg$debug("Skip subsetting of task '%s'", task$id)
  }

  # pass test ids w/o cloning
  if (!is.null(test_row_ids)) {
    prev_test = task$row_roles$test
    on.exit({
      task$row_roles$test = prev_test
    }, add = TRUE)
    task$row_roles$test = test_row_ids
  }

  if (mode == "train") learner$state = list()

  lg$debug("Calling %s method of Learner '%s' on task '%s' with %i observations",
    mode, learner$id, task$id, task$nrow, learner = learner$clone())

  # call train_wrapper with encapsulation
  result = encapsulate(learner$encapsulate["train"],
    .f = train_wrapper,
    .args = list(learner = learner, task = task),
    .pkgs = learner$packages,
    .seed = NA_integer_,
    .timeout = learner$timeout["train"]
  )

  log = append_log(NULL, "train", result$log$class, result$log$msg)
  train_time = result$elapsed

  learner$state = insert_named(learner$state, list(
    model = result$result,
    log = log,
    train_time = train_time,
    param_vals = learner$param_set$values,
    task_hash = task$hash,
    feature_names = task$feature_names,
    mlr3_version = mlr_reflections$package_version
  ))

  if (is.null(result$result)) {
    lg$debug("Learner '%s' on task '%s' failed to %s a model",
      learner$id, task$id, mode, learner = learner$clone(), messages = result$log$msg)
  } else {
    lg$debug("Learner '%s' on task '%s' succeeded to %s a model",
      learner$id, task$id, mode, learner = learner$clone(), result = result$result, messages = result$log$msg)
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

    result = get_private(learner)$.predict(task)
    as_prediction_data(result, task = task, check = TRUE, train_task = learner$state$train_task)
  }

  assert_task(task)
  assert_learner(learner)

  # ensure that required packages are installed
  require_namespaces(learner$packages)

  if (getOption("mlr3.warn_version_mismatch", TRUE)) {
    v_train = learner$state$mlr3_version
    v_predict = mlr_reflections$package_version

    if (!is.null(v_train) && v_train != v_predict) {
      warningf("Detected version mismatch: Learner '%s' has been trained with mlr3 version '%s', not matching currently installed version '%s'",
        learner$id, v_train, v_predict)
    }
  }

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
    return(as_prediction_data(named_list(), task = task, row_ids = integer(), check = TRUE, train_task = learner$state$train_task))
  }

  if (is.null(learner$state$model)) {
    lg$debug("Learner '%s' has no model stored",
      learner$id, learner = learner$clone())

    pdata = NULL
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

    pdata = result$result
    learner$state$log = append_log(learner$state$log, "predict", result$log$class, result$log$msg)
    learner$state$predict_time = result$elapsed

    lg$debug("Learner '%s' returned an object of class '%s'",
      learner$id, class(pdata)[1L], learner = learner$clone(), prediction_data = pdata, messages = result$log$msg)
  }


  fb = learner$fallback
  if (!is.null(fb)) {
    predict_fb = function(row_ids) {
      fb = assert_learner(as_learner(fb))
      fb$predict_type = learner$predict_type
      fb$state = learner$state$fallback_state
      as_prediction_data(fb$predict(task, row_ids), task, row_ids, check = TRUE, train_task = learner$state$train_task)
    }


    if (is.null(pdata)) {
      lg$debug("Creating new Prediction using fallback '%s'",
        fb$id, learner = fb$clone())

      learner$state$log = append_log(learner$state$log, "predict", "output", "Using fallback learner for predictions")
      pdata = predict_fb(task$row_ids)
    } else {
      miss_ids = is_missing_prediction_data(pdata)

      lg$debug("Imputing %i/%i predictions using fallback '%s'",
        length(miss_ids), length(pdata$row_ids), fb$id, learner = fb$clone())

      if (length(miss_ids)) {
        learner$state$log = append_log(learner$state$log, "predict", "output", "Using fallback learner to impute predictions")

        pdata = c(pdata, predict_fb(miss_ids), keep_duplicates = FALSE)
      }
    }
  }

  return(pdata)
}


workhorse = function(iteration, task, learner, resampling, param_values = NULL, lgr_threshold, store_models = FALSE, pb = NULL, mode = "train", is_sequential = TRUE) {
  if (!is.null(pb)) {
    pb(sprintf("%s|%s|i:%i", task$id, learner$id, iteration))
  }

  # reduce data.table and blas threads to 1
  if (!is_sequential) {
    setDTthreads(1, restore_after_fork = TRUE)
    old_blas_threads = blas_get_num_procs()
    on.exit(blas_set_num_threads(old_blas_threads), add = TRUE)
    blas_set_num_threads(1)
  }

  # restore logger thresholds
  for (package in names(lgr_threshold)) {
    logger = lgr::get_logger(package)
    threshold = lgr_threshold[package]
    logger$set_threshold(threshold)
  }

  lg$info("%s learner '%s' on task '%s' (iter %i/%i)",
    if (mode == "train") "Applying" else "Hotstarting", learner$id, task$id, iteration, resampling$iters)

  sets = list(
    train = resampling$train_set(iteration),
    test = resampling$test_set(iteration),
    holdout = task$row_roles$holdout
  )

  # train model
  learner = learner$clone()
  if (length(param_values)) {
    learner$param_set$values = list()
    learner$param_set$set_values(.values = param_values)
  }
  learner_hash = learner$hash
  learner = learner_train(learner, task, sets[["train"]], sets[["test"]], mode = mode)

  # predict for each set
  sets = sets[learner$predict_sets]
  pdatas = Map(function(set, row_ids) {
    lg$debug("Creating Prediction for predict set '%s'", set)
    learner_predict(learner, task, row_ids)
  }, set = names(sets), row_ids = sets)
  pdatas = discard(pdatas, is.null)

  if (!store_models) {
    lg$debug("Erasing stored model for learner '%s'", learner$id)
    learner$state$model = NULL
  }

  list(learner_state = learner$state, prediction = pdatas, param_values = learner$param_set$values, learner_hash = learner_hash)
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
