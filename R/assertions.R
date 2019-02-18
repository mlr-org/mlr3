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
#' @rdname mlr_assertions
assert_task = function(task, .var.name = vname(task)) {
  assert_class(task, "Task", .var.name = .var.name)
}

#' @export
#' @param learner ([Learner]).
#' @rdname mlr_assertions
assert_learner = function(learner, task = NULL, .var.name = vname(learner)) {
  assert_class(learner, "Learner", .var.name = .var.name)
  if (!is.null(task)) {
    if (!identical(task$task_type, learner$task_type)) {
      stopf("Learner '%s' is not compatible with type of task '%s' (type: %s)",
        learner$id, task$id, task$task_type)
    }
  }
  invisible(learner)
}

#' @export
#' @param measure ([Measure]).
#' @param predict_types (`character()`). Vector of predict types provided by the experiment/learner.
#' @rdname mlr_assertions
assert_measure = function(measure, task = NULL, predict_types = NULL, .var.name = vname(measure)) {
  assert_class(measure, "Measure", .var.name = .var.name)

  if (!is.null(task)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != task$task_type)
      stopf("Measure '%s' is not compatible with type of task '%s' (type: %s)",
        measure$id, task$id, task$task_type)

    miss = setdiff(measure$task_properties, task$properties)
    if (length(miss))
      stopf("Measure '%s' needs task properties: %s",
        measure$id, str_collapse(miss))
  }

  if (!is.null(predict_types)) {
    if (!is_scalar_na(measure$predict_type) && measure$predict_type %nin% predict_types)
      stopf("Measure '%s' needs predict_type '%s'", measure$id, measure$predict_type)
  }

  measure
}

#' @export
#' @param measures (list of [Measure]).
#' @rdname mlr_assertions
assert_measures = function(measures, task = NULL, predict_types = NULL, .var.name = vname(measures)) {
  assert_list(measures, min.len = 1L, .var.name = .var.name)
  lapply(measures, assert_measure, task = task, predict_types = predict_types)
}

#' @export
#' @param resampling ([Resampling]).
#' @rdname mlr_assertions
assert_resampling = function(resampling, instantiated = NULL, .var.name = vname(resampling)) {
  assert_class(resampling, "Resampling", .var.name = .var.name)
  if (!is.null(instantiated)) {
    if (instantiated && !resampling$is_instantiated)
      stopf("Resampling '%s' must be instantiated", resampling$id)
    if (!instantiated && resampling$is_instantiated)
      stopf("Resampling '%s' may not be instantiated", resampling$id)
  }
  invisible(resampling)
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
  if (is.double(row_ids))
    row_ids = as.integer(row_ids)
  if (!is.null(type) && typeof(row_ids) != type)
    stopf("Assertion on '%s' failed: Must be of type '%s', not '%s'", .var.name, type, typeof(row_ids))
  invisible(row_ids)
}

assert_set = function(x, empty = TRUE, .var.name = vname(x)) {
  assert_character(x, min.len = as.integer(!empty), any.missing = FALSE, min.chars = 1L, unique = TRUE, .var.name = .var.name)
}

assert_range = function(range, .var.name = vname(range)) {
  assert_numeric(range, len = 2L, any.missing = FALSE, .var.name = .var.name)
  if (diff(range) <= 0)
    stopf("Invalid range specified. First value (%f) must be greater than second value (%f)", range[1L], range[2L])
  range
}

assert_sorted_subset = function(x, choices, ..., .var.name = vname(x)) {
  assert_subset(x, choices, ..., .var.name = .var.name)
  x[match(choices, x, nomatch = 0L)]
}
