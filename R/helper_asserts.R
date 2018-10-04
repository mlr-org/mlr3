assert_task = function(task) {
  assert_class(task, "Task")
}

assert_backend = function(b) {
  assert_class(b, "DataBackend")
}

assert_experiment = function(experiment) {
  assert_class(experiment, "Experiment")
}

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

assert_measure = function(measure, task = NULL, learner = NULL) {
  assert_class(measure, "Measure")

  if (!is.null(task)) {
    if (measure$task_type != task$task_type)
      stopf("Measure '%s'  is not compatible with type of task '%s' (type: %s)",
        measure$id, task$id, task$task_type)

    miss = setdiff(measure$task_properties, task$properties)
    if (length(miss))
      stopf("Measure '%s' needs task properties: %s",
        measure$id, stri_head(miss))
  }

  if (!is.null(learner)) {
    if (measure$task_type != learner$task_type)
      stopf("Measure '%s'  is not compatible with type of learner '%s' (type: %s)",
        measure$id, learner$id, learner$task_type)

    miss = setdiff(measure$task_properties, learner$properties)
    if (length(miss))
      stopf("Measure '%s' needs learner '%s' to have the properties: %s",
        measure$id, learner$id, stri_head(miss))

    if (!is_scalar_na(measure$predict_type) && measure$predict_type != learner$predict_type)
      stopf("Measure '%s' needs learner '%s' to have predict_type '%s'",
        measure$id, learner$id, measure$predict_type)
  }

  measure
}

assert_measures = function(measures, task = NULL, learner = NULL) {
  assert_list(measures, min.len = 1L)
  lapply(measures, assert_measure, task = task, learner = learner)
}

assert_resampling = function(resampling) {
  assert_class(resampling, "Resampling")
}

assert_resampling_index = function(r, i) {
  if (!r$is_instantiated)
    stopf("Resampling %s has not been instantiated yet", r$id)
  assert_int(i, lower = 1L, upper = r$iters, coerce = TRUE)
}

assert_resample_result = function(rr) {
  assert_class(rr, "ResampleResult")
}

assert_id = function(id) {
  assert_string(id, min.chars = 1L)
}

assert_set = function(x, empty = TRUE) {
  assert_character(x, min.len = as.integer(!empty), any.missing = FALSE, min.chars = 1L, unique = TRUE)
}

assert_par_set = function(par_set) {
  assert_class(par_set, "ParamSet")
}

assert_par_vals = function(par_vals, par_set) {
  assert_list(par_vals, names = "unique", any.missing = FALSE)
  assert_subset(names(par_vals), par_set$ids)
  par_vals
}

assert_range = function(range) {
  assert_numeric(range, len = 2L, any.missing = FALSE)
  if (diff(range) <= 0)
    stopf("Invalid range specified. First value (%f) must be greater than second value (%f)", range[1L], range[2L])
  range
}

assert_unique_hashes = function(x) {
  assert_list(x)
  hashes = vcapply(x, "[[", "hash")
  if (anyDuplicated(hashes))
    stopf("Duplicated elements found in '%s'", deparse(substitute(x)))
  invisible(x)
}
