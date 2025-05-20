learner_train = function(learner, task, train_row_ids = NULL, test_row_ids = NULL, mode = "train") {
  # This wrapper calls learner$.train, and additionally performs some basic
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


    # In order to avoid unnecessary (un-)marshaling steps,
    # we already extract the internal tuned values and validation scores here.
    # They should only operate on the model and the param_vals so the
    # information above should be enough.
    # In the future, we might want to refactor this, so the extractors get directly
    # called with the model and param_vals
    learner$state$model = model
    learner$state$param_vals = learner$param_set$values

    # Extract internal valid scores and tuned values if applicable.
    internal_valid_scores = if (!is.null(get0("validate", learner)) &&
      exists(".extract_internal_valid_scores", get_private(learner))) {
      get_private(learner)$.extract_internal_valid_scores()
    }

    internal_tuned_values = if (exists(".extract_internal_tuned_values", get_private(learner))) {
      get_private(learner)$.extract_internal_tuned_values()
    }

    if (learner$encapsulation[["train"]] == "callr") {
      model = marshal_model(model, inplace = TRUE)
    }

    list(
      model = model,
      internal_valid_scores = internal_valid_scores,
      internal_tuned_values = internal_tuned_values
    )
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

    task_private = get_private(task)
    prev_use = task_private$.row_roles$use
    on.exit({
      task_private$.row_roles$use = prev_use
    }, add = TRUE)
    task_private$.row_roles$use  = train_row_ids
    task_private$.row_hash = NULL
    task_private$.hash = NULL
  } else {
    lg$debug("Skip subsetting of task '%s'", task$id)
  }

  # handle the internal validation task
  validate = get0("validate", learner)
  prev_valid = task$internal_valid_task
  on.exit({
    task$internal_valid_task = prev_valid
  }, add = TRUE)

  # depending on the validate parameter, create the internal validation task (if needed)
  # modifies the task in place
  create_internal_valid_task(validate, task, test_row_ids, prev_valid, learner)
  if (!is.null(task$internal_valid_task) && !task$internal_valid_task$nrow) {
    stopf("Internal validation task for task '%s' has 0 observations", task$id)
  }

  if (mode == "train") learner$state = list()

  lg$debug("Calling %s method of Learner '%s' on task '%s' with %i observations",
    mode, learner$id, task$id, task$nrow, learner = learner$clone())

  # call train_wrapper with encapsulation
  result = encapsulate(learner$encapsulation["train"],
    .f = train_wrapper,
    .args = list(learner = learner, task = task),
    .pkgs = learner$packages,
    .seed = NA_integer_,
    .timeout = learner$timeout["train"]
  )

  log = append_log(NULL, "train", result$log$class, result$log$msg)
  train_time = result$elapsed

  learner$state = set_class(insert_named(learner$state, list(
    model = result$result$model,
    log = log,
    train_time = train_time,
    param_vals = learner$param_set$values,
    task_hash = task$hash,
    feature_names = task$feature_names,
    validate = get0("validate", learner),
    mlr3_version = mlr_reflections$package_version
  )), c("learner_state", "list"))

  # store the results of the internal tuning / internal validation in the learner's state
  # otherwise this information is only available with store_models = TRUE
  if (!is.null(result$result$internal_valid_scores)) {
    learner$state$internal_valid_scores = result$result$internal_valid_scores
    learner$state$internal_valid_task_hash = task$internal_valid_task$hash
  }

  learner$state$internal_tuned_values = result$result$internal_tuned_values

  if (is.null(result$result$model)) {
    lg$info("Learner '%s' on task '%s' failed to %s a model",
      learner$id, task$id, mode, learner = learner$clone(), messages = result$log$msg)
  } else {
    lg$debug("Learner '%s' on task '%s' succeeded to %s a model",
      learner$id, task$id, mode, learner = learner$clone(), result = result$result$model, messages = result$log$msg)
  }

  # fit fallback learner
  fb = learner$fallback
  if (!is.null(fb)) {
    lg$info("Calling train method of fallback '%s' on task '%s' with %i observations",
      fb$id, task$id, task$nrow, learner = fb$clone())

    fb = assert_learner(as_learner(fb))
    require_namespaces(fb$packages)
    fb$train(task)
    learner$state$fallback_state = fb$state

    lg$debug("Fitted fallback learner '%s'",
      fb$id, learner = fb$clone())
  }


  list(
    learner = learner,
    internal_valid_task_ids = if (!is.null(validate)) task$internal_valid_task$row_ids
  )
}

learner_predict = function(learner, task, row_ids = NULL) {
  # This wrapper calls learner$predict, and additionally performs some basic
  # checks that the prediction was successful.
  # Exceptions here are possibly encapsulated, so that they get captured and turned into log messages.

  predict_wrapper = function(task, learner) {
    # default method does nothing
    learner$model = unmarshal_model(learner$model, inplace = TRUE)
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

    task_private = get_private(task)
    prev_use = task_private$.row_roles$use
    on.exit({
      task_private$.row_roles$use  = prev_use
    }, add = TRUE)
    task_private$.row_roles$use  = row_ids
  } else {
    lg$debug("Skip subsetting of task '%s'", task$id)
  }

  if (task$nrow == 0L) {
    # return an empty prediction object, #421
    lg$debug("No observations in task, returning empty prediction data", task = task)
    learner$state$log = append_log(learner$state$log, "predict", "output", "No data to predict on, create empty prediction")
    return(create_empty_prediction_data(task, learner))
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

    if (isTRUE(all.equal(learner$encapsulation[["predict"]], "callr"))) {
      learner$model = marshal_model(learner$model, inplace = TRUE)
    }

    result = encapsulate(
      learner$encapsulation["predict"],
      .f = predict_wrapper,
      .args = list(task = task, learner = learner),
      .pkgs = learner$packages,
      .seed = NA_integer_,
      .timeout = learner$timeout["predict"]
    )

    pdata = result$result
    learner$state$log = append_log(learner$state$log, "predict", result$log$class, result$log$msg)
    learner$state$predict_time = sum(learner$state$predict_time, result$elapsed)

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


workhorse = function(
  iteration,
  task,
  learner,
  resampling,
  param_values = NULL,
  lgr_index,
  store_models = FALSE,
  pb = NULL,
  mode = "train",
  is_sequential = TRUE,
  unmarshal = TRUE,
  callbacks = NULL
  ) {
  ctx = ContextResample$new(task, learner, resampling, iteration)

  call_back("on_resample_begin", callbacks, ctx)

  if (!is.null(pb)) {
    pb(sprintf("%s|%s|i:%i", task$id, learner$id, iteration))
  }
  if ("internal_valid" %chin% learner$predict_sets && is.null(task$internal_valid_task) && is.null(get0("validate", learner))) {
    stopf("Cannot set the predict_type field of learner '%s' to 'internal_valid' if there is no internal validation task configured", learner$id)
  }

  # restore settings on the workers
  if (!is_sequential) {
    # reduce data.table threads to 1
    setDTthreads(1, restore_after_fork = TRUE)

    # reduce blas threads to 1
    # RhpcBLASctl is licensed under AGPL and therefore should be in suggest #1023
    if (require_namespaces("RhpcBLASctl", quietly = TRUE)) {
      old_blas_threads = RhpcBLASctl::blas_get_num_procs()
      on.exit(RhpcBLASctl::blas_set_num_threads(old_blas_threads), add = TRUE)
      RhpcBLASctl::blas_set_num_threads(1)
    } else { # try the bare minimum to disable threading of the most popular blas implementations
      old_blas = Sys.getenv("OPENBLAS_NUM_THREADS")
      old_mkl = Sys.getenv("MKL_NUM_THREADS")
      Sys.setenv(OPENBLAS_NUM_THREADS = 1)
      Sys.setenv(MKL_NUM_THREADS = 1)

      on.exit({
        Sys.setenv(OPENBLAS_NUM_THREADS = old_blas)
        Sys.setenv(MKL_NUM_THREADS = old_mkl)
      }, add = TRUE)
    }

    # restore logger thresholds
    # skip inherited thresholds
    lgr_index = lgr_index[!lgr_index$threshold_inherited, ]
    mapply(function(name, threshold) {
      logger = lgr::get_logger(name)
      logger$set_threshold(threshold)
    }, lgr_index$name, lgr_index$threshold)
  }

  lg$info("%s learner '%s' on task '%s' (iter %i/%i)",
    if (mode == "train") "Applying" else "Hotstarting", learner$id, task$id, iteration, resampling$iters)

  sets = list(
    train = resampling$train_set(iteration),
    test = resampling$test_set(iteration)
  )

  # train model
  # use `learner` reference instead of `ctx$learner` to avoid going through the active binding
  ctx$learner = learner = ctx$learner$clone()
  if (length(param_values)) {
    learner$param_set$values = list()
    learner$param_set$set_values(.values = param_values)
  }
  learner_hash = learner$hash

  validate = get0("validate", learner)

  test_set = if (identical(validate, "test")) sets$test

  call_back("on_resample_before_train", callbacks, ctx)

  train_result = learner_train(learner, task, sets[["train"]], test_set, mode = mode)
  ctx$learner = learner = train_result$learner

  # process the model so it can be used for prediction (e.g. marshal for callr prediction), but also
  # keep a copy of the model in current form in case this is the format that we want to send back to the main process
  # and not the format that we need for prediction
  model_copy_or_null = process_model_before_predict(
    learner = learner, store_models = store_models, is_sequential = is_sequential, unmarshal = unmarshal
  )

  # predict for each set
  predict_sets = learner$predict_sets

  # creates the tasks and row_ids for all selected predict sets
  pred_data = prediction_tasks_and_sets(task, train_result, validate, sets, predict_sets)
  call_back("on_resample_before_predict", callbacks, ctx)

  pdatas = Map(function(set, row_ids, task) {
    lg$debug("Creating Prediction for predict set '%s'", set)

    learner_predict(learner, task, row_ids)
  }, set = predict_sets, row_ids = pred_data$sets, task = pred_data$tasks)

  if (!length(predict_sets)) {
    learner$state$predict_time = 0L
  }
  ctx$pdatas = discard(pdatas, is.null)

  # set the model slot after prediction so it can be sent back to the main process
  process_model_after_predict(
    learner = learner, store_models = store_models, is_sequential = is_sequential, model_copy = model_copy_or_null,
    unmarshal = unmarshal
  )

  call_back("on_resample_end", callbacks, ctx)

  if (!store_models) {
    lg$debug("Erasing stored model for learner '%s'", learner$id)
    learner$state$model = NULL
  }

  learner_state = set_class(learner$state, c("learner_state", "list"))

  list(
    learner_state = learner_state,
    prediction = ctx$pdatas,
    param_values = learner$param_set$values,
    learner_hash = learner_hash,
    data_extra = ctx$data_extra)
}

# creates the tasks and row ids for the selected predict sets
prediction_tasks_and_sets = function(task, train_result, validate, sets, predict_sets) {
  predict_sets = predict_sets[predict_sets %in% mlr_reflections$predict_sets]
  tasks = list(train = task, test = task)
  if ("internal_valid" %nin% predict_sets) {
    return(list(tasks = tasks[predict_sets], sets = sets[predict_sets]))
  }

  if ("internal_valid" %chin% predict_sets) {
    if (is.numeric(validate) || identical(validate, "test")) {
      # in this scenario, the internal_valid_task was created during learner_train, which means that it used the
      # primary task. The selected ids are returned via the train result
      tasks$internal_valid = task
      sets$internal_valid = train_result$internal_valid_task_ids
    } else {
      # the predefined internal_valid_task was used
      tasks$internal_valid = task$internal_valid_task
      sets$internal_valid = task$internal_valid_task$row_ids
    }
  }

  list(tasks = tasks[predict_sets], sets = sets[predict_sets])
}

process_model_before_predict = function(learner, store_models, is_sequential, unmarshal) {
  # there are three states of the model that have to be considered to minimize how often we marshal a model:
  # 1. the current form: is it marshaled or not?
  # 2. the form for prediction: do we need to marshal it?
  # 3. the final form that is returned: does it have to be marshaled?
  #
  # and also, do we even need to send it back at all?

  currently_marshaled = is_marshaled_model(learner$model)
  predict_needs_marshaling = isTRUE(all.equal(learner$encapsulation[["predict"]], "callr"))
  final_needs_marshaling = !is_sequential || !unmarshal

  # the only scenario in which we keep a copy is when we now have the model in the correct form but need to transform
  # it for prediction
  keep_copy = store_models & (currently_marshaled == final_needs_marshaling) && (currently_marshaled != predict_needs_marshaling)

  if (!keep_copy) {
    # here we either
    # * don't return the model at all --> no copy
    # * the predict form is equal to the final form --> no copy
    # * we do store models but the current form is not the final form --> no copy
    if (predict_needs_marshaling) {
      learner$model = marshal_model(learner$model, inplace = TRUE)
    } else {
      learner$model = unmarshal_model(learner$model, inplace = TRUE)
    }
    return(NULL)
  }

  # here, we do store models, the current form is the final form and the current form is not the predict form
  # in order to avoid a marshaling cycle, we therefore keep a copy of the current model and then continue to
  # process the model for prediction

  # note that even though learner_predict takes care of the marshaling itself, it does do it in-place
  # Because we here have a copy of the model, we transform it NOT in-place. This is important because otherwise
  # we will mess up our copy

  model_copy = learner$model
  if (predict_needs_marshaling) {
    learner$model = marshal_model(learner$model, inplace = FALSE)
  } else {
    learner$model = unmarshal_model(learner$model, inplace = FALSE)
  }
  return(model_copy)
}

process_model_after_predict = function(learner, store_models, is_sequential, unmarshal, model_copy) {
  if (store_models && !is.null(model_copy)) {
    # we created a copy of the model to avoid additional marshaling cycles
    learner$model = model_copy
  } else if (store_models &&  !is_sequential || !unmarshal) {
    # no copy was created, here we make sure that we return the model the way the user wants it
    learner$model = marshal_model(learner$model, inplace = TRUE)
  }
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
    pwalk(list(stage, class, msg), function(s, c, m) {
      if (c == "error") lg$error("%s: %s", s, m)
      if (c == "warning") lg$warn("%s: %s", s, m)
    })
    log = rbindlist(list(log, data.table(stage = stage, class = class, msg = msg)), use.names = TRUE)
  }

  log
}

create_internal_valid_task = function(validate, task, test_row_ids, prev_valid, learner) {
  if (is.null(validate)) {
    task$internal_valid_task = NULL
    return(task)
  }

  # Otherwise, predict_set = "internal_valid" is ambiguous
  if (!is.null(prev_valid) && (is.numeric(validate) || identical(validate, "test"))) {
    stopf("Parameter 'validate' of Learner '%s' cannot be set to 'test' or a ratio when internal_valid_task is present", learner$id)
  }

  if (is.character(validate)) {
    if (validate == "predefined") {
      if (is.null(task$internal_valid_task)) {
        stopf("Parameter 'validate' is set to 'predefined' but no internal validation task is present. This commonly happens in GraphLearners and can be avoided by configuring the validation data for the  GraphLearner via `set_validate(<glrn>, validate = <value>)`. See https://mlr3book.mlr-org.com/chapters/chapter15/predsets_valid_inttune.html for more information.")
      }
      if (!identical(task$target_names, task$internal_valid_task$target_names)) {
        stopf("Internal validation task '%s' has different target names than primary task '%s', did you modify the task after creating the internal validation task?",
          task$internal_valid_task$id, task$id)
      }
      if (!test_permutation(task$feature_names, task$internal_valid_task$feature_names)) {
        stopf("Internal validation task '%s' has different features than primary task '%s', did you modify the task after creating the internal validation task?",
          task$internal_valid_task$id, task$id)
      }
      return(task)
    } else { # validate is "test"
      if (is.null(test_row_ids)) {
        stopf("Parameter 'validate' cannot be set to 'test' when calling train manually.")
      }
      # at this point, the train rows are already set to the train set, i.e. we don't have to remove the test ids
      # from the primary task (this would cause bugs for resamplings with overlapping train and test set)
      valid_task = task$clone(deep = TRUE)
      valid_task$row_roles$use = test_row_ids
      task$internal_valid_task = valid_task
      return(task)
    }

   return(task)
  }

  # validate is numeric
  task$internal_valid_task = partition(task, ratio = 1 - validate)$test
  return(task)
}
