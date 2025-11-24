#' @title Assertion for mlr3 Objects
#'
#' @description
#' Functions intended to be used in packages extending \pkg{mlr3}.
#' Most assertion functions ensure the right class attribute, and optionally additional properties.
#' Additionally, the following compound assertions are implemented:
#'
#' * `assert_learnable(task, learner)`\cr
#'   ([Task], [Learner]) -> `NULL`\cr
#'   Checks if the learner is applicable to the task.
#'   This includes type checks on the type, the feature types, and properties.
#'
#' If an assertion fails, an exception is raised.
#' Otherwise, the input object is returned invisibly.
#'
#' @name mlr_assertions
#' @keywords internal
NULL


#' @export
#' @param b ([DataBackend]).
#' @rdname mlr_assertions
assert_backend = function(b, .var.name = vname(b)) {
  assert_class(b, "DataBackend", .var.name = .var.name)
}


#' @param task ([Task]).
#' @template param_feature_types
#' @param task_properties (`character()`)\cr
#'   Set of required task properties.
#' @rdname mlr_assertions
#' @export
assert_task = function(task, task_type = NULL, feature_types = NULL, task_properties = NULL, .var.name = vname(task)) {
  assert_class(task, "Task", .var.name = .var.name)

  if (!is.null(task_type) && task$task_type != task_type) {
    error_input("Task '%s' must have type '%s'", task$id, task_type)
  }

  if (!is.null(feature_types)) {
    tmp = setdiff(task$feature_types$type, feature_types)
    if (length(tmp)) {
      error_input("Task '%s' has the following unsupported feature types: %s", task$id, str_collapse(tmp))
    }
  }

  if (!is.null(task_properties)) {
    tmp = setdiff(task_properties, task$properties)
    if (length(tmp)) {
      error_input("Task '%s' is missing the following properties: %s", task$id, str_collapse(tmp))
    }
  }

  invisible(task)
}


#' @export
#' @param tasks (list of [Task]).
#' @rdname mlr_assertions
assert_tasks = function(tasks, task_type = NULL, feature_types = NULL, task_properties = NULL, .var.name = vname(tasks)) {
  invisible(lapply(tasks, assert_task, task_type = task_type, feature_types = feature_types, task_properties = task_properties, .var.name = .var.name))
}


#' @export
#' @param learner ([Learner]).
#' @param task_type (`character(1)`).
#' @rdname mlr_assertions
assert_learner = function(learner, task = NULL, task_type = NULL, properties = character(), .var.name = vname(learner)) {
  assert_class(learner, "Learner", .var.name = .var.name)

  task_type = task_type %??% task$task_type
  # check on class(learner) does not work with GraphLearner and AutoTuner
  # check on learner$task_type does not work with TaskUnsupervised
  if (!test_matching_task_type(task_type, learner, "learner")) {
    error_input("Learner '%s' must have task type '%s'", learner$id, task_type)
  }

  if (length(properties)) {
    miss = setdiff(properties, learner$properties)
    if (length(miss)) {
      error_input("Learner '%s' must have the properties: %s", learner$id, str_collapse(miss))
    }
  }

  invisible(learner)
}

test_matching_task_type = function(task_type, object, class) {
  if (is.null(task_type) || object$task_type == task_type) {
    return(TRUE)
  }

  cl_task_type = fget_key(mlr_reflections$task_types, task_type, class, "type")
  if (inherits(object, cl_task_type)) {
    return(TRUE)
  }

  cl_object = fget_key(mlr_reflections$task_types, object$task_type, class, "type")
  cl_task_type == cl_object
}


#' @export
#' @param learners (list of [Learner]).
#' @rdname mlr_assertions
assert_learners = function(learners, task = NULL, task_type = NULL, properties = character(), unique_ids = FALSE, .var.name = vname(learners)) {
  if (unique_ids) {
    ids = map_chr(learners, "id")
    if (!test_character(ids, unique = TRUE)) {
      error_input("Learners need to have unique IDs: %s", str_collapse(ids))
    }
  }
  invisible(lapply(learners, assert_learner, task = task, task_type = NULL, properties = properties, .var.name = .var.name))
}

# this does not check the validation task, as this is only possible once the validation set is known,
# which happens during worker(), so it cannot be checked before that
assert_task_learner = function(task, learner, param_values = NULL, cols = NULL) {
  pars = learner$param_set$get_values(type = "only_token", check_required = FALSE)
  # remove pars that are covered by param_values
  pars = pars[names(pars) %nin% names(param_values)]
  if (length(pars) > 0) {
    error_config("%s cannot be trained with TuneToken present in hyperparameter: %s", format_angle_brackets(learner), str_collapse(names(pars)))
  }
  # check on class(learner) does not work with GraphLearner and AutoTuner
  # check on learner$task_type does not work with TaskUnsupervised

  if (!test_matching_task_type(task$task_type, learner, "learner")) {
    error_input("Type '%s' of %s does not match type '%s' of %s",
      task$task_type, format_angle_brackets(task), learner$task_type, format_angle_brackets(learner))
  }

  tmp = setdiff(task$feature_types$type, learner$feature_types)
  if (length(tmp) > 0) {
    error_input("%s has the following unsupported feature types: %s", format_angle_brackets(task), str_collapse(tmp))
  }

  if ("missings" %nin% learner$properties) {
    miss = task$missings(cols = cols) > 0L
    if (any(miss)) {
      error_input("Task '%s' has missing values in column(s) %s, but learner '%s' does not support this",
        task$id, str_collapse(names(miss)[miss], quote = "'"), learner$id)
    }
  }

  if ("offset" %in% task$properties && "offset" %nin% learner$properties) {
    warning_input("Task '%s' has offset, but learner '%s' does not support this, so it will be ignored",
             task$id, learner$id)
  }

  tmp = mlr_reflections$task_mandatory_properties[[task$task_type]]
  if (length(tmp)) {
    tmp = setdiff(intersect(task$properties, tmp), learner$properties)
    if (length(tmp)) {
      error_input("Task '%s' has property '%s', but learner '%s' does not support that",
        task$id, tmp[1L], learner$id)
    }
  }

  validate = get0("validate", learner)
  if (!is.null(task$internal_valid_task) && (is.numeric(validate) || identical(validate, "test"))) {
    error_config("Parameter 'validate' of Learner '%s' cannot be set to 'test' or a ratio when internal_valid_task is present, remove it first", learner$id)
  }
}

#' @export
#' @param param_values (`list()`)\cr
#'  TuneToken are not allowed in the parameter set of the learner.
#'  If the `param_values` overwrite the TuneToken, the assertion will pass.
#' @rdname mlr_assertions
assert_learnable = function(task, learner, param_values = NULL) {
  if (task$task_type == "unsupervised") {
    error_input("%s cannot be trained with %s", format_angle_brackets(learner), format_angle_brackets(task))
  }
  # we only need to check whether the learner wants to error on weights in training,
  # since weights_learner are always ignored during prediction.
  if (learner$use_weights == "error" && "weights_learner" %in% task$properties) {
    error_config("%s cannot be trained with weights in %s%s", format_angle_brackets(learner), format_angle_brackets(task),
      if ("weights" %in% learner$properties) {
        " since 'use_weights' was set to 'error'."
      } else {
        " since the Learner does not support weights.\nYou may set 'use_weights' to 'ignore' if you want the Learner to ignore weights."
      })
  }
  assert_task_learner(task, learner, param_values)
}

#' @export
#' @rdname mlr_assertions
assert_predictable = function(task, learner) {
  if (!is.null(learner$state$train_task)) {
    train_task = learner$state$train_task
    cols_train = train_task$feature_names
    cols_predict = task$feature_names

    if (!test_permutation(cols_train, cols_predict)) {
      error_input("Learner '%s' has received tasks with different columns in train and predict.", learner$id)
    }

    ids = fget_keys(train_task$col_info, i = cols_train, j = "id", key = "id")
    train_type = fget_keys(train_task$col_info, i = ids, j = "type", key = "id")
    train_levels = fget_keys(train_task$col_info, i = ids, j = "levels", key = "id")
    predict_type = fget_keys(task$col_info, i = ids, j = "type", key = "id")
    predict_levels = fget_keys(task$col_info, i = ids, j = "levels", key = "id")


    ok = all(train_type == predict_type)

    if ("new_levels" %nin% learner$properties) {
      ok = ok && all(pmap_lgl(list(x = train_levels, y = predict_levels), identical))
    }

    if (!ok) {
      error_input("Learner '%s' received task with different column info (feature type or factor level ordering) during train and predict.", learner$id)
    }
  }

  assert_task_learner(task, learner, cols = task$feature_names)
}



#' @export
#' @param measure ([Measure]).
#' @param prediction ([Prediction]).
#' @rdname mlr_assertions
assert_measure = function(measure, task = NULL, learner = NULL, prediction = NULL, .var.name = vname(measure)) {
  assert_class(measure, "Measure", .var.name = .var.name)

  if (measure$use_weights == "error" && (!is.null(prediction$weights) || "weights_measure" %chin% task$properties)) {
    error_input("%s cannot be evaluated with weights%s%s", format_angle_brackets(measure), if (!is.null(task)) paste0(" in ", format_angle_brackets(task)) else "",
      if ("weights" %in% measure$properties) {
        " since 'use_weights' was set to 'error'."
      } else {
        " since the Measure does not support weights.\nYou may set 'use_weights' to 'ignore' if you want the Measure to ignore weights."
      }
    )
  }

  if (!is.null(task)) {

    if (!is_scalar_na(measure$task_type) && !test_matching_task_type(task$task_type, measure, "measure")) {
      error_input("Measure '%s' is not compatible with type '%s' of task '%s'",
        measure$id, task$task_type, task$id)
    }

    if (measure$check_prerequisites != "ignore") {
      miss = setdiff(measure$task_properties, task$properties)
      if (length(miss) > 0) {
        warning_config("Measure '%s' is missing properties %s of task '%s'",
          measure$id, str_collapse(miss, quote = "'"), task$id)
      }
    }
  }

  if (!is.null(learner)) {

    if (!is_scalar_na(measure$task_type) && measure$task_type != learner$task_type) {
      error_input("Measure '%s' is not compatible with type '%s' of learner '%s'",
        measure$id, learner$task_type, learner$id)
    }

    if (!is_scalar_na(measure$predict_type) && measure$check_prerequisites != "ignore") {
      predict_types = mlr_reflections$learner_predict_types[[learner$task_type]][[learner$predict_type]]
      if (measure$predict_type %nin% predict_types) {
        warning_config("Measure '%s' is missing predict type '%s' of learner '%s'", measure$id, measure$predict_type, learner$id)
      }
    }

    if (measure$check_prerequisites != "ignore") {
      miss = setdiff(measure$predict_sets, learner$predict_sets)
      if (length(miss) > 0) {

        warning_config("Measure '%s' needs predict sets %s, but learner '%s' only predicted on sets %s",
          measure$id, str_collapse(miss, quote = "'"), learner$id, str_collapse(learner$predict_sets, quote = "'"))
      }
    }
  }

  if (!is.null(prediction) && is.null(learner)) {
    # same as above but works without learner e.g. measure$score(prediction)
    if (measure$check_prerequisites != "ignore" && measure$predict_type %nin% prediction$predict_types) {
      warning_config("Measure '%s' is missing predict type '%s' of prediction", measure$id, measure$predict_type)
    }
  }

  invisible(measure)
}

#' @export
#' @param measure ([Measure]).
#' @param prediction ([Prediction]).
#' @rdname mlr_assertions
assert_scorable = function(measure, task, learner, prediction = NULL, .var.name = vname(measure)) {
  if ("requires_model" %chin% measure$properties && is.null(learner$model)) {
    error_input("Measure '%s' requires the trained model", measure$id)
  }

  if ("requires_model" %chin% measure$properties && is_marshaled_model(learner$model)) {
    error_input("Measure '%s' requires the trained model, but model is in marshaled form", measure$id)
  }

  assert_measure(measure, task = task, learner = learner, prediction = prediction, .var.name = .var.name)
}

#' @export
#' @param measures (list of [Measure]).
#' @rdname mlr_assertions
assert_measures = function(measures, task = NULL, learner = NULL, .var.name = vname(measures)) {
  lapply(measures, assert_measure, task = task, learner = learner, .var.name = .var.name)
  if (anyDuplicated(ids(measures))) {
    error_input("Measures need to have unique IDs")
  }
  invisible(measures)
}

#' @export
#' @param resampling ([Resampling]).
#' @rdname mlr_assertions
assert_resampling = function(resampling, instantiated = NULL, .var.name = vname(resampling)) {
  assert_class(resampling, "Resampling", .var.name = .var.name)

  if (!is.null(instantiated)) {
    if (instantiated && !resampling$is_instantiated) {
      error_input("Resampling '%s' must be instantiated", resampling$id)
    }
    if (!instantiated && resampling$is_instantiated) {
      error_input("Resampling '%s' may not be instantiated", resampling$id)
    }
  }

  invisible(resampling)
}


#' @export
#' @param resamplings (list of [Resampling]).
#' @rdname mlr_assertions
assert_resamplings = function(resamplings, instantiated = NULL, .var.name = vname(resamplings)) {
  invisible(lapply(resamplings, assert_resampling, instantiated = instantiated, .var.name = .var.name))
}


#' @export
#' @param prediction ([Prediction]).
#' @rdname mlr_assertions
assert_prediction = function(prediction, .var.name = vname(prediction), null.ok = FALSE) {
  if (null.ok && is.null(prediction)) return(prediction)
  assert_class(prediction, "Prediction", .var.name = .var.name)
}


#' @export
#' @param rr ([ResampleResult]).
#' @rdname mlr_assertions
assert_resample_result = function(rr, .var.name = vname(rr)) {
  assert_class(rr, "ResampleResult", .var.name = .var.name)
}


#' @export
#' @param bmr ([BenchmarkResult]).
#' @rdname mlr_assertions
assert_benchmark_result = function(bmr, .var.name = vname(bmr)) {
  assert_class(bmr, "BenchmarkResult", .var.name = .var.name)
}


assert_set = function(x, empty = TRUE, .var.name = vname(x)) {
  assert_character(x, min.len = as.integer(!empty), any.missing = FALSE, min.chars = 1L, unique = TRUE, .var.name = .var.name)
}


assert_range = function(range, .var.name = vname(range)) {
  assert_numeric(range, len = 2L, any.missing = FALSE, .var.name = .var.name)

  if (diff(range) <= 0) {
    error_input("Invalid range specified. First value (%f) must be greater than second value (%f)", range[1L], range[2L])
  }

  invisible(range)
}


#' @export
#' @template param_row_ids
#' @param task ([Task])\cr
#'   Task to check if row ids exist in it.
#' @rdname mlr_assertions
assert_row_ids = function(row_ids, task = NULL, null.ok = FALSE, .var.name = vname(row_ids)) {
  row_ids = assert_integerish(row_ids, coerce = TRUE, null.ok = null.ok)
  if (!is.null(task)) {
    if (any(row_ids %nin% task$row_ids)) {
      error_input("The provided row ids do not exist in task '%s'", task$id)
    }
  }
  invisible(row_ids)
}

#' @export
#' @param task ([Task]).
#' @rdname mlr_assertions
assert_has_backend = function(task) {
  if (is.null(task$backend)) {
    error_input("The backend of Task '%s' has been removed. Set `store_backends` to `TRUE` during model fitting to conserve it.", task$id)
  }
}

# assertion to ensure a helpful error message
assert_prediction_count = function(actual, expected, type) {
  if (actual != expected) {
    if (actual < expected) {
      error_learner_predict("Predicted %s not complete, %s for %i observations is missing",
        type, type, expected - actual)
    } else {
      error_learner_predict("Predicted %s contains %i additional predictions without matching rows",
        type, actual - expected)
    }
  }
}

assert_row_sums = function(prob) {
  for (i in seq_row(prob)) {
    x = prob[i, , drop = TRUE]
    n_missing = count_missing(x)
    if (n_missing > 0L) {
      if (n_missing < length(x)) {
        error_input("Probabilities for observation %i are partly missing", i)
      }
    } else {
      s = sum(x)
      if (abs(s - 1) > 0.001) {
        error_input("Probabilities for observation %i do sum up to %f != 1", i, s)
      }
    }
  }
}


#' @export
#' @param learner ([Learner]).
#' @param quantile_response (`logical(1)`)\cr
#'   Whether to check if the quantile response is set.
#'   If `TRUE`, the learner must have the `$quantile_response` field set.
#' @rdname mlr_assertions
assert_quantiles = function(learner, quantile_response = FALSE) {

  if (is.null(learner$quantiles)) {
    error_config("Quantiles must be set via `$quantiles`")
  }

  if (quantile_response && is.null(learner$quantile_response)) {
    error_config("Quantile response must be set via `$quantile_response`")
  }

  invisible(learner)
}

assert_param_values = function(x, n_learners = NULL, .var.name = vname(x)) {
  assert_list(x, len = n_learners, .var.name = .var.name)

  ok = every(x, function(x) {
    test_list(x) && every(x, test_list, names = "unique", null.ok = TRUE)
  })

  if (!ok) {
    error_input("'%s' must be a three-time nested list and the most inner list must be named", .var.name)
  }
  invisible(x)
}

#' @title Assert Empty Ellipsis
#' @description
#' Assert that `...` arguments are empty.
#' Use this function in S3-methods to ensure that misspelling of arguments does not go unnoticed.
#' @param ... (any)\cr
#'    Ellipsis arguments to check.
#' @keywords internal
#' @return `NULL`
#' @export
assert_empty_ellipsis = function(...) {
  nx = ...length()
  if (nx == 0L) {
    return(NULL)
  }
  names = ...names()
  if (is.null(names)) {
    error_input("Received %i unnamed argument that was not used.", nx)
  }
  names2 = names[nzchar(names)]
  if (length(names2) == length(names)) {
    error_input(
      "Received the following named arguments that were unused: %s.",
      toString(names2)
    )
  }
  error_input(
    "Received unused arguments: %i unnamed, as well as named arguments %s.",
    length(names) - length(names2), toString(names2)
  )
}
