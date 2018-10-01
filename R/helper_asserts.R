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
    if (!identical(task$type, learner$task_type)) {
      stopf("Learner '%s' (type: %s) is not compatible with task '%s' (type: %s)",
        learner$id, learner$task_type, task$id, task$type)
    }
  }
  invisible(learner)
}

assert_measure = function(measure) {
  assert_class(measure, "Measure")
}

assert_measures = function(measures) {
  assert_list(measures, "Measure", min.len = 1L)
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

assert_packages = function(packages) {
  assert_character(packages, any.missing = FALSE, min.chars = 1L, unique = TRUE)
}

assert_properties = function(properties) {
  assert_character(properties, any.missing = FALSE, min.chars = 1L, unique = TRUE)
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
