map_check = function(x, checker, ...) {
  for (xi in x) {
    msg = checker(xi, ...)
    if (!isTRUE(msg))
      return(msg)
  }
  TRUE
}

check_backend = function(b) {
  check_r6(b, "DataBackend")
}


check_task = function(task, task_type = NULL, feature_types = NULL, task_properties = NULL) {
  if (!inherits(task, "Task")) {
    return("Must be of class 'Task'")
  }

  if (!is.null(task_type) && task$task_type != task_type) {
    return(sprintf("Task '%s' must have type '%s'", task$id, task_type))
  }

  if (!is.null(feature_types)) {
    tmp = setdiff(task$feature_types$type, feature_types)
    if (length(tmp)) {
      return(sprintf("Task '%s' has the following unsupported feature types: %s", task$id, str_collapse(tmp)))
    }
  }

  if (!is.null(task_properties)) {
    tmp = setdiff(task_properties, task$properties)
    if (length(tmp)) {
      return(sprintf("Task '%s' is missing the following properties: %s", task$id, str_collapse(tmp)))
    }
  }

  TRUE
}

check_tasks = function(tasks, task_type = NULL, feature_types = NULL, task_properties = NULL) {
  map_check(tasks, check_task, task_type = task_type, feature_types = feature_types, task_properties = task_properties)
}

check_learner = function(learner, task = NULL, properties = character()) {
  if (!inherits(learner, "Learner")) {
    return("Must be of class 'Learner'")
  }

  if (!is.null(task)) {
    if (!identical(task$task_type, learner$task_type)) {
      return(sprintf("Learner '%s' is not compatible with type '%s' of task '%s'",
        learner$id, task$task_type, task$id))
    }
  }

  if (length(properties)) {
    miss = setdiff(properties, learner$properties)
    if (length(miss)) {
      return(sprintf("Learner '%s' must have the properties: %s", learner$id, str_collapse(miss)))
    }
  }

  TRUE
}

check_learners = function(learners, task = NULL, properties = character()) {
  map_check(learners, check_learner, task = task, properties = properties)
}

check_measure = function(measure, task = NULL, learner = NULL) {
  if (!inherits(measure, "Measure")) {
    return("Must be of class 'Measure'")
  }

  if (!is.null(task)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != task$task_type) {
      return(sprintf("Measure '%s' is not compatible with type '%s' of task '%s'",
        measure$id, task$task_type, task$id))
    }

    miss = setdiff(measure$task_properties, task$properties)
    if (length(miss)) {
      return(sprintf("Measure '%s' needs task properties: %s", measure$id, str_collapse(miss)))
    }
  }

  if (!is.null(learner)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != learner$task_type) {
      return(sprintf("Measure '%s' is not compatible with type '%s' of learner '%s'",
        measure$id, learner$task_type, learner$id))
    }

    if (!is_scalar_na(measure$predict_type)) {
      predict_types = mlr_reflections$learner_predict_types[[learner$task_type]][[learner$predict_type]]
      if (measure$predict_type %nin% predict_types) {
        return(sprintf("Measure '%s' needs predict_type '%s'", measure$id, measure$predict_type))
      }
    }
  }

  TRUE
}

check_measures = function(measures, task = NULL, learner = NULL) {
  map_check(measures, check_measure, task = task, learner = learner)
}

check_resampling = function(resampling, instantiated = NULL) {
  if (!inherits(resampling, "Resampling")) {
    return("Must be of class 'Resampling'")
  }

  if (!is.null(instantiated)) {
    if (instantiated && !resampling$is_instantiated) {
      return(sprintf("Resampling '%s' must be instantiated", resampling$id))
    }
    if (!instantiated && resampling$is_instantiated) {
      return(sprintf("Resampling '%s' may not be instantiated", resampling$id))
    }
  }

  TRUE
}

check_resamplings = function(resamplings, instantiated = NULL) {
  map_check(resamplings, check_resampling, instantiated = instantiated)
}

check_prediction = function(prediction) {
  check_class(prediction, "Prediction")
}

check_resample_result = function(rr) {
  check_class(rr, "ResampleResult")
}

check_benchmark_result = function(bmr) {
  check_class(bmr, "BenchmarkResult")
}

check_set = function(x, empty = TRUE) {
  check_character(x, min.len = as.integer(!empty), any.missing = FALSE, min.chars = 1L, unique = TRUE)
}

check_range = function(range) {
  msg = check_numeric(range, len = 2L, any.missing = FALSE)
  if (!isTRUE(msg)) {
    return(msg)
  }

  if (diff(range) <= 0) {
    return(sprintf("Invalid range specified. First value (%f) must be greater than second value (%f)", range[1L], range[2L]))
  }

  TRUE
}

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
assert_backend = makeAssertionFunction(check_backend)

#' @param task :: [Task].
#' @param feature_types :: `character()`\cr
#'   Set of allowed feature types.
#' @param task_properties :: `character()`\cr
#'   Set of required task properties.
#' @rdname mlr_assertions
#' @export
assert_task = makeAssertionFunction(check_task)

#' @export
#' @param tasks :: list of [Task].
#' @rdname mlr_assertions
assert_tasks = makeAssertionFunction(check_tasks)

#' @export
#' @param learner :: [Learner].
#' @rdname mlr_assertions
assert_learner = makeAssertionFunction(check_learner)

#' @export
#' @param learners :: list of [Learner].
#' @rdname mlr_assertions
assert_learners = makeAssertionFunction(check_learners)

#' @export
#' @param measure :: [Measure].
#' @rdname mlr_assertions
assert_measure = makeAssertionFunction(check_measure)

#' @export
#' @param measures :: list of [Measure].
#' @rdname mlr_assertions
assert_measures = makeAssertionFunction(check_measures)

#' @export
#' @param resampling :: [Resampling].
#' @rdname mlr_assertions
assert_resampling = makeAssertionFunction(check_resampling)

#' @export
#' @param resamplings :: list of [Resampling].
#' @rdname mlr_assertions
assert_resamplings = makeAssertionFunction(check_resamplings)

#' @export
#' @param prediction :: [Prediction].
#' @rdname mlr_assertions
assert_prediction = makeAssertionFunction(check_prediction)

#' @export
#' @param resample_result :: [ResampleResult].
#' @rdname mlr_assertions
assert_resample_result = makeAssertionFunction(check_resample_result)

#' @export
#' @param bmr :: [BenchmarkResult].
#' @rdname mlr_assertions
assert_benchmark_result = makeAssertionFunction(check_benchmark_result)

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

assert_set = makeAssertionFunction(check_set)

assert_range = makeAssertionFunction(check_range)
