#' @title Assertion for mlr3 Objects
#'
#' @description
#' Functions intended to be used in packages extending \pkg{mlr3}.
#' All functions assert on the respective class, and optionally additional properties.
#' If an assertion fails, an exception is raised.
#' Otherwise, the input object is returned invisibly.
#'
#' @name mlr_assertions
#' @keywords internal
NULL


#' @export
#' @param b :: [DataBackend].
#' @rdname mlr_assertions
assert_backend = function(b, .var.name = vname(b)) {
  assert_r6(b, .var.name = .var.name)
}


#' @param task :: [Task].
#' @param feature_types :: `character()`\cr
#'   Set of allowed feature types.
#' @param task_properties :: `character()`\cr
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
#' @param tasks :: list of [Task].
#' @rdname mlr_assertions
assert_tasks = function(tasks, task_type = NULL, feature_types = NULL, task_properties = NULL, .var.name = vname(tasks)) {
  invisible(lapply(tasks, assert_task, task_type = task_type, feature_types = feature_types, task_properties = task_properties, .var.name = .var.name))
}


#' @export
#' @param learner :: [Learner].
#' @rdname mlr_assertions
assert_learner = function(learner, task = NULL, properties = character(), .var.name = vname(learner)) {
  assert_class(learner, "Learner", .var.name = .var.name)

  if (!is.null(task)) {
    if (!identical(task$task_type, learner$task_type)) {
      stopf("Learner '%s' is not compatible with type '%s' of task '%s'",
        learner$id, task$task_type, task$id)
    }
  }

  if (length(properties)) {
    miss = setdiff(properties, learner$properties)
    if (length(miss)) {
      stopf("Learner '%s' must have the properties: %s", learner$id, str_collapse(miss))
    }
  }

  invisible(learner)
}


#' @export
#' @param learners :: list of [Learner].
#' @rdname mlr_assertions
assert_learners = function(learners, task = NULL, properties = character(), .var.name = vname(learners)) {
  invisible(lapply(learners, assert_learner, task = task, properties = properties, .var.name = .var.name))
}


#' @export
#' @param measure :: [Measure].
#' @rdname mlr_assertions
assert_measure = function(measure, task = NULL, learner = NULL, .var.name = vname(measure)) {
  assert_class(measure, "Measure", .var.name = .var.name)

  if (!is.null(task)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != task$task_type) {
      stopf("Measure '%s' is not compatible with type '%s' of task '%s'",
        measure$id, task$task_type, task$id)
    }

    miss = setdiff(measure$task_properties, task$properties)
    if (length(miss)) {
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
    if (length(miss)) {
      stopf("Measure '%s' needs predict set '%s'", measure$id, str_collapse(miss))
    }
  }

  invisible(measure)
}


#' @export
#' @param measures :: list of [Measure].
#' @rdname mlr_assertions
assert_measures = function(measures, task = NULL, learner = NULL, .var.name = vname(measures)) {
  lapply(measures, assert_measure, task = task, learner = learner, .var.name = .var.name)
  if (anyDuplicated(ids(measures)))
    stopf("Measures need to have unique IDs")
  invisible(measures)
}


#' @export
#' @param resampling :: [Resampling].
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
#' @param resamplings :: list of [Resampling].
#' @rdname mlr_assertions
assert_resamplings = function(resamplings, instantiated = NULL, .var.name = vname(resamplings)) {
  invisible(lapply(resamplings, assert_resampling, instantiated = instantiated, .var.name = .var.name))
}


#' @export
#' @param prediction :: [Prediction].
#' @rdname mlr_assertions
assert_prediction = function(prediction, .var.name = vname(prediction)) {
  assert_class(prediction, "Prediction", .var.name = .var.name)
}


#' @export
#' @param resample_result :: [ResampleResult].
#' @rdname mlr_assertions
assert_resample_result = function(rr, .var.name = vname(rr)) {
  assert_class(rr, "ResampleResult", .var.name = .var.name)
}


#' @export
#' @param bmr :: [BenchmarkResult].
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
#' @param row_ids :: `vector()`.
#' @rdname mlr_assertions
assert_row_ids = function(row_ids, type = NULL, .var.name = vname(row_ids)) {
  # TODO: make this a proper check function
  # TODO: coercion in checkmate does not work here
  qassert(row_ids, c("X", "S[1,]"), .var.name = .var.name)
  if (is.double(row_ids)) {
    row_ids = as.integer(row_ids)
  }
  if (!is.null(type) && typeof(row_ids) != type) {
    stopf("Assertion on '%s' failed: Must be of type '%s', not '%s'", .var.name, type, typeof(row_ids))
  }

  invisible(row_ids)
}
