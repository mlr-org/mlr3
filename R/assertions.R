#' @title Assertion for mlr3 objects
#'
#' @description
#' Functions intended to be used in packages extending \pkg{mlr3}.
#'
#' @name mlr_assertions
#' @keywords internal
NULL


#' @export
#' @param b \[[DataBackend]\].
#' @rdname mlr_assertions
assert_backend = function(b) {
  assert_class(b, "DataBackend")
}

#' @export
#' @param e \[[Experiment]\].
#' @rdname mlr_assertions
assert_experiment = function(e) {
  assert_class(e, "Experiment")
}

#' @export
#' @param task \[[Task]\].
#' @rdname mlr_assertions
assert_task = function(task) {
  assert_class(task, "Task")
}

#' @export
#' @param learner \[[Learner]\].
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
#' @param measure \[[Measure]\].
#' @rdname mlr_assertions
assert_measure = function(measure, task = NULL, learner = NULL) {
  assert_class(measure, "Measure")

  if (!is.null(task)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != task$task_type)
      stopf("Measure '%s'  is not compatible with type of task '%s' (type: %s)",
        measure$id, task$id, task$task_type)

    miss = setdiff(measure$task_properties, task$properties)
    if (length(miss))
      stopf("Measure '%s' needs task properties: %s",
        measure$id, paste0(miss, collapse = ", "))
  }

  if (!is.null(learner)) {
    if (!is_scalar_na(measure$task_type) && measure$task_type != learner$task_type)
      stopf("Measure '%s'  is not compatible with type of learner '%s' (type: %s)",
        measure$id, learner$id, learner$task_type)

    miss = setdiff(measure$task_properties, learner$properties)
    if (length(miss))
      stopf("Measure '%s' needs learner '%s' to have the properties: %s",
        measure$id, learner$id, paste0(miss, collapse = ", "))

    if (!is_scalar_na(measure$predict_type) && measure$predict_type != learner$predict_type)
      stopf("Measure '%s' needs learner '%s' to have predict_type '%s'",
        measure$id, learner$id, measure$predict_type)
  }

  measure
}

#' @export
#' @param measures \[`list` of [Measure]\].
#' @rdname mlr_assertions
assert_measures = function(measures, task = NULL, learner = NULL) {
  assert_list(measures, min.len = 1L)
  lapply(measures, assert_measure, task = task, learner = learner)
}

#' @export
#' @param resampling \[[Resampling]\].
#' @rdname mlr_assertions
assert_resampling = function(resampling) {
  assert_class(resampling, "Resampling")
}

#' @export
#' @param resample_result \[[ResampleResult]\].
#' @rdname mlr_assertions
assert_resample_result = function(resample_result) {
  assert_class(resample_result, "ResampleResult")
}

#' @export
#' @param bmr \[[BenchmarkResult]\].
#' @rdname mlr_assertions
assert_benchmark_result = function(bmr) {
  assert_class(bmr, "BenchmarkResult")
}

#' @export
#' @param param_set \[[paradox::ParamSet()]\].
#' @rdname mlr_assertions
assert_param_set = function(param_set) {
  assert_class(param_set, "ParamSet")
}

#' @export
#' @param param_vals \[`named list`\].
#' @rdname mlr_assertions
assert_param_vals = function(param_vals, param_set) {
  assert_list(param_vals, names = "unique", any.missing = FALSE)
  assert_subset(names(param_vals), param_set$ids)
  param_vals
}

assert_id = function(id) {
  assert_string(id, min.chars = 1L)
}

assert_set = function(x, empty = TRUE) {
  assert_character(x, min.len = as.integer(!empty), any.missing = FALSE, min.chars = 1L, unique = TRUE)
}

assert_resampling_index = function(resampling, i) {
  if (!resampling$is_instantiated)
    stopf("Resampling %s has not been instantiated yet", resampling$id)
  assert_int(i, lower = 1L, upper = resampling$iters, coerce = TRUE)
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
