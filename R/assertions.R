#' @title Assertion for mlr3 Objects
#'
#' @description
#' Functions intended to be used in packages extending \pkg{mlr3}.
#' Most assertion functions ensure the right class attrbiture, and optionally additional properties.
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
    stopf("Task '%s' must have type '%s'", task$id, task_type)
  }

  if (!is.null(feature_types)) {
    tmp = setdiff(task$feature_types$type, feature_types)
    if (length(tmp)) {
      stopf("Task '%s' has the following unsupported feature types: %s", task$id, str_collapse(tmp))
    }
  }

  if (!is.null(task_properties)) {
    tmp = setdiff(task_properties, task$properties)
    if (length(tmp)) {
      stopf("Task '%s' is missing the following properties: %s", task$id, str_collapse(tmp))
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
#' @rdname mlr_assertions
assert_learner = function(learner, task = NULL, properties = character(), .var.name = vname(learner)) {
  assert_class(learner, "Learner", .var.name = .var.name)

  if (length(properties)) {
    miss = setdiff(properties, learner$properties)
    if (length(miss)) {
      stopf("Learner '%s' must have the properties: %s", learner$id, str_collapse(miss))
    }
  }

  invisible(learner)
}


#' @export
#' @param learners (list of [Learner]).
#' @rdname mlr_assertions
assert_learners = function(learners, task = NULL, properties = character(), .var.name = vname(learners)) {
  invisible(lapply(learners, assert_learner, task = task, properties = properties, .var.name = .var.name))
}

assert_task_learner = function(task, learner, cols = NULL) {
  pars = learner$param_set$get_values(type = "only_token")
  if (length(pars) > 0) {
    stopf("%s cannot be trained with TuneToken present in hyperparameter: %s", learner$format(), str_collapse(names(pars)))
  }

  if (task$task_type != learner$task_type) {
    stopf("Type '%s' of %s does not match type '%s' of %s",
      task$task_type, task$format(), learner$task_type, learner$format())
  }

  tmp = setdiff(task$feature_types$type, learner$feature_types)
  if (length(tmp) > 0) {
    stopf("%s has the following unsupported feature types: %s", task$format(), str_collapse(tmp))
  }

  if ("missings" %nin% learner$properties) {
    miss = task$missings(cols = cols) > 0L
    if (any(miss)) {
      stopf("Task '%s' has missing values in column(s) %s, but learner '%s' does not support this",
        task$id, str_collapse(names(miss)[miss], quote = "'"), learner$id)
    }
  }
}

#' @export
#' @rdname mlr_assertions
assert_learnable = function(task, learner) {
  assert_task_learner(task, learner)
}

#' @export
#' @rdname mlr_assertions
assert_predictable = function(task, learner) {
  assert_task_learner(task, learner, cols = task$feature_names)
}



#' @export
#' @param measure ([Measure]).
#' @rdname mlr_assertions
assert_measure = function(measure, task = NULL, learner = NULL, .var.name = vname(measure)) {
  assert_class(measure, "Measure", .var.name = .var.name)

  if (!is.null(task)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != task$task_type) {
      stopf("Measure '%s' is not compatible with type '%s' of task '%s'",
        measure$id, task$task_type, task$id)
    }

    miss = setdiff(measure$task_properties, task$properties)
    if (length(miss) > 0) {
      stopf("Measure '%s' needs task properties: %s", measure$id, str_collapse(miss))
    }
  }

  if (!is.null(learner)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != learner$task_type) {
      stopf("Measure '%s' is not compatible with type '%s' of learner '%s'",
        measure$id, learner$task_type, learner$id)
    }

    if (!is_scalar_na(measure$predict_type)) {
      predict_types = mlr_reflections$learner_predict_types[[learner$task_type]][[learner$predict_type]]
      if (measure$predict_type %nin% predict_types) {
        stopf("Measure '%s' needs predict_type '%s'", measure$id, measure$predict_type)
      }
    }

    miss = setdiff(measure$predict_sets, learner$predict_sets)
    if (length(miss) > 0) {
      stopf("Measure '%s' needs predict set %s, but learner '%s' only predicted on sets %s",
        measure$id, str_collapse(miss, quote = "'"), learner$id, str_collapse(learner$predict_sets, quote = "'"))
    }
  }

  invisible(measure)
}


#' @export
#' @param measures (list of [Measure]).
#' @rdname mlr_assertions
assert_measures = function(measures, task = NULL, learner = NULL, .var.name = vname(measures)) {
  lapply(measures, assert_measure, task = task, learner = learner, .var.name = .var.name)
  if (anyDuplicated(ids(measures))) {
    stopf("Measures need to have unique IDs")
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
      stopf("Resampling '%s' must be instantiated", resampling$id)
    }
    if (!instantiated && resampling$is_instantiated) {
      stopf("Resampling '%s' may not be instantiated", resampling$id)
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
assert_prediction = function(prediction, .var.name = vname(prediction)) {
  assert_class(prediction, "Prediction", .var.name = .var.name)
}


#' @export
#' @param resample_result ([ResampleResult]).
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
    stopf("Invalid range specified. First value (%f) must be greater than second value (%f)", range[1L], range[2L])
  }

  invisible(range)
}


#' @export
#' @param row_ids (`numeric()`).
#' @rdname mlr_assertions
assert_row_ids = function(row_ids, null.ok = FALSE, .var.name = vname(row_ids)) {
  assert_integerish(row_ids, coerce = TRUE, null.ok = null.ok)
}

assert_has_backend = function(task) {
  if (is.null(task$backend)) {
    stopf("The backend of Task '%s' has been removed. Set `store_backends` to `TRUE` during model fitting to conserve it.", task$id)
  }
}

# assertion to ensure a helpful error message
assert_prediction_count = function(actual, expected, type) {
  if (actual != expected) {
    if (actual < expected) {
      stopf("Predicted %s not complete, %s for %i observations is missing",
        type, type, expected - actual)
    } else {
      stopf("Predicted %s contains %i additional predictions without matching rows",
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
        stopf("Probabilities for observation %i are partly missing", i)
      }
    } else {
      s = sum(x)
      if (abs(s - 1) > 0.001) {
        stopf("Probabilities for observation %i do sum up to %f != 1", i, s)
      }
    }
  }
}

assert_same_task_type = function(objs) {
  task_types = unique(map_chr(objs, "task_type"))
  if (length(task_types) > 1L) {
    stopf("Multiple task types detected, but mixing types is not supported: %s", str_collapse(task_types))
  }
}
