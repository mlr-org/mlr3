#' @title Assertion for mlr3 Objects
#' @description
#' Functions intended to be used in packages extending \pkg{mlr3}.
#' @name mlr_assertions
#' @keywords internal
NULL

#' @export
#' @param b ([DataBackend]).
#' @rdname mlr_assertions
assert_backend = function(b) {
  assert_class(b, "DataBackend")
}

#' @export
#' @param e ([Experiment]).
#' @rdname mlr_assertions
assert_experiment = function(e) {
  assert_class(e, "Experiment")
}

#' @export
#' @param task ([Task]).
#' @rdname mlr_assertions
assert_task = function(task) {
  assert_class(task, "Task")
}

#' @export
#' @param learner ([Learner]).
#' @rdname mlr_assertions
assert_learner = function(learner, task = NULL) {
  assert_class(learner, "Learner")
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
#' @rdname mlr_assertions
assert_measure = function(measure, task = NULL, prediction = NULL) {
  assert_class(measure, "Measure")

  if (!is.null(task)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != task$task_type)
      stopf("Measure '%s'  is not compatible with type of task '%s' (type: %s)",
        measure$id, task$id, task$task_type)

    miss = setdiff(measure$task_properties, task$properties)
    if (length(miss))
      stopf("Measure '%s' needs task properties: %s",
        measure$id, str_collapse(miss))
  }

  if (!is.null(prediction)) {
    if (!is_scalar_na(measure$predict_type) && measure$predict_type %nin% prediction$predict_types)
      stopf("Measure '%s' needs predict_type '%s'", measure$id, measure$predict_type)
  }

  measure
}

#' @export
#' @param measures (list of [Measure]).
#' @rdname mlr_assertions
assert_measures = function(measures, task = NULL, prediction = NULL) {
  assert_list(measures, min.len = 1L)
  lapply(measures, assert_measure, task = task, prediction = prediction)
}

#' @export
#' @param resampling ([Resampling]).
#' @rdname mlr_assertions
assert_resampling = function(resampling, instantiated = NULL) {
  assert_class(resampling, "Resampling")
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
assert_resample_result = function(resample_result) {
  assert_class(resample_result, "ResampleResult")
}

#' @export
#' @param bmr ([BenchmarkResult]).
#' @rdname mlr_assertions
assert_benchmark_result = function(bmr) {
  assert_class(bmr, "BenchmarkResult")
}

#' @export
#' @param param_set ([paradox::ParamSet]).
#' @rdname mlr_assertions
assert_param_set = function(param_set) {
  assert_class(param_set, "ParamSet")
}

#' @export
#' @param param_vals (named `list()`). Its values are checked for feasibility w.r.t. the provided [paradox::ParamSet()].
#' @rdname mlr_assertions
assert_param_vals = function(param_vals, param_set) {
  assert_list(param_vals, names = "unique", any.missing = FALSE)
  param_set$assert(param_vals, .var.name = "param_vals")
  required = names(which(map_lgl(param_set$tags, is.element, el = "required")))
  required = setdiff(required, names(param_vals))
  if (length(required))
    stopf("Missing required parameters: %s", str_collapse(required))
  param_vals
}

assert_id = function(id) {
  assert_string(id, min.chars = 1L)
}

assert_set = function(x, empty = TRUE) {
  assert_character(x, min.len = as.integer(!empty), any.missing = FALSE, min.chars = 1L, unique = TRUE)
}

assert_range = function(range) {
  assert_numeric(range, len = 2L, any.missing = FALSE)
  if (diff(range) <= 0)
    stopf("Invalid range specified. First value (%f) must be greater than second value (%f)", range[1L], range[2L])
  range
}

assert_unique_hashes = function(x) {
  assert_list(x)
  hashes = hashes(x)
  if (anyDuplicated(hashes))
    stopf("Duplicated elements found in '%s'", deparse(substitute(x)))
  invisible(x)
}
