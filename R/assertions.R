#' @title Assertion for mlr3 Objects
#' @description
#' Functions intended to be used in packages extending \pkg{mlr3}.
#' @name mlr_assertions
#' @keywords internal
NULL

#' @export
#' @param b ([DataBackend]).
#' @rdname mlr_assertions
assert_backend = function(b, .var.name = vname(b)) {
  assert_class(b, "DataBackend", .var.name = .var.name)
}

#' @export
#' @param e ([Experiment]).
#' @rdname mlr_assertions
assert_experiment = function(e, .var.name = vname(e)) {
  assert_class(e, "Experiment", .var.name = .var.name)
}

#' @export
#' @param task ([Task]).
#' @param feature_types (`character()`)\cr
#'   Set of allowed feature types.
#' @param task_properties (`character()`)\cr
#'   Set of required task properties.
#' @rdname mlr_assertions
assert_task = function(task, feature_types = NULL, task_properties = NULL, clone = FALSE) {
  task = cast_from_dict(task, "Task", mlr_tasks, clone, FALSE)[[1L]]

  if (!is.null(feature_types)) {
    tmp = setdiff(task$feature_types$type, feature_types)
    if (length(tmp)) {
      stopf("Task has the following unsupported feature types: %s", str_collapse(tmp))
    }
  }

  if (!is.null(task_properties)) {
    tmp = setdiff(task_properties, task$properties)
    if (length(tmp)) {
      stopf("Task is missing the following properties: %s", str_collapse(tmp))
    }
  }

  task
}

#' @export
#' @param tasks (list of [Task]).
#' @rdname mlr_assertions
assert_tasks = function(tasks, feature_types = NULL, task_properties = NULL, clone = FALSE) {
  tasks = cast_from_dict(tasks, "Task", mlr_tasks, clone, TRUE)
  lapply(tasks, assert_task, feature_types = feature_types, task_properties = task_properties)
}

#' @export
#' @param learner ([Learner]).
#' @rdname mlr_assertions
assert_learner = function(learner, task = NULL, properties = character(0L), clone = FALSE) {
  learner = cast_from_dict(learner, "Learner", mlr_learners, clone, FALSE)[[1L]]

  if (!is.null(task)) {
    if (!identical(task$task_type, learner$task_type)) {
      stopf("Learner '%s' is not compatible with type of task '%s' (type: %s)",
        learner$id, task$id, task$task_type)
    }
  }

  if (length(properties)) {
    miss = setdiff(properties, learner$properties)
    if (length(miss)) {
      stopf("Learner '%s' must have the properties: %s", learner$id, str_collapse(miss))
    }
  }

  learner
}

#' @export
#' @param learners (list of [Learner]).
#' @rdname mlr_assertions
assert_learners = function(learners, task = NULL, properties = character(0L), clone = FALSE) {
  learners = cast_from_dict(learners, "Learner", mlr_learners, clone, TRUE)
  lapply(learners, assert_learner, task = task, properties = properties)
}

#' @export
#' @param measure ([Measure]).
#' @param predict_types (`character()`). Vector of predict types provided by the experiment/learner.
#' @rdname mlr_assertions
assert_measure = function(measure, task = NULL, predict_types = NULL, clone = FALSE) {
  measure = cast_from_dict(measure, "Measure", mlr_measures, clone, FALSE)[[1L]]

  if (!is.null(task)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != task$task_type) {
      stopf("Measure '%s' is not compatible with type of task '%s' (type: %s)",
        measure$id, task$id, task$task_type)
    }

    miss = setdiff(measure$task_properties, task$properties)
    if (length(miss)) {
      stopf("Measure '%s' needs task properties: %s",
        measure$id, str_collapse(miss))
    }
  }

  if (!is.null(predict_types)) {
    if (!is_scalar_na(measure$predict_type) && measure$predict_type %nin% predict_types) {
      stopf("Measure '%s' needs predict_type '%s'", measure$id, measure$predict_type)
    }
  }

  return(measure)
}

#' @export
#' @param measures (list of [Measure]).
#' @rdname mlr_assertions
assert_measures = function(measures, task = NULL, predict_types = NULL, clone = FALSE) {
  measures = cast_from_dict(measures, "Measure", mlr_measures, clone, TRUE)
  lapply(measures, assert_measure, task = task, predict_types = predict_types)
}

#' @export
#' @param resampling ([Resampling]).
#' @rdname mlr_assertions
assert_resampling = function(resampling, instantiated = NULL, clone = FALSE) {
  resampling = cast_from_dict(resampling, "Resampling", mlr_resamplings, clone, FALSE)[[1L]]
  if (!is.null(instantiated)) {
    if (instantiated && !resampling$is_instantiated) {
      stopf("Resampling '%s' must be instantiated", resampling$id)
    }
    if (!instantiated && resampling$is_instantiated) {
      stopf("Resampling '%s' may not be instantiated", resampling$id)
    }
  }

  return(resampling)
}

#' @export
#' @param resamplings (list of [Resampling]).
#' @rdname mlr_assertions
assert_resamplings = function(resamplings, instantiated = NULL, clone = FALSE) {
  resamplings = cast_from_dict(resamplings, "Resampling", mlr_resamplings, clone, TRUE)
  lapply(resamplings, assert_resampling, instantiated = instantiated)
}

#' @export
#' @param resample_result ([ResampleResult]).
#' @rdname mlr_assertions
assert_resample_result = function(resample_result, .var.name = vname(resample_result)) {
  assert_class(resample_result, "ResampleResult", .var.name = .var.name)
}

#' @export
#' @param bmr ([BenchmarkResult]).
#' @rdname mlr_assertions
assert_benchmark_result = function(bmr, .var.name = vname(bmr)) {
  assert_class(bmr, "BenchmarkResult", .var.name = .var.name)
}

#' @export
#' @param param_set ([paradox::ParamSet]).
#' @rdname mlr_assertions
assert_param_set = function(param_set, .var.name = vname(param_set)) {
  assert_class(param_set, "ParamSet", .var.name = .var.name)
}

#' @export
#' @param id (`character(1)`).
#' @rdname mlr_assertions
assert_id = function(id, .var.name = vname(id)) {
  assert_string(id, min.chars = 1L, .var.name = .var.name)
}

assert_row_ids = function(row_ids, type = NULL, .var.name = vname(row_ids)) {
  qassert(row_ids, c("X", "S[1,]"), .var.name = .var.name)
  if (is.double(row_ids)) {
    row_ids = as.integer(row_ids)
  }
  if (!is.null(type) && typeof(row_ids) != type) {
    stopf("Assertion on '%s' failed: Must be of type '%s', not '%s'", .var.name, type, typeof(row_ids))
  }
  return(row_ids)
}

assert_set = function(x, empty = TRUE, .var.name = vname(x)) {
  assert_character(x, min.len = as.integer(!empty), any.missing = FALSE, min.chars = 1L, unique = TRUE, .var.name = .var.name)
}

assert_range = function(range, .var.name = vname(range)) {
  assert_numeric(range, len = 2L, any.missing = FALSE, .var.name = .var.name)
  if (diff(range) <= 0) {
    stopf("Invalid range specified. First value (%f) must be greater than second value (%f)", range[1L], range[2L])
  }
  range
}

assert_sorted_subset = function(x, choices, ..., .var.name = vname(x)) {
  assert_subset(x, choices, ..., .var.name = .var.name)
  x[match(choices, x, nomatch = 0L)]
}
