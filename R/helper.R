translate_types = function(x) {
  r_types = mlr_reflections$task_feature_types
  p_types = names(mlr_reflections$task_feature_types)
  factor(map_values(x, r_types, p_types), levels = p_types)
}


allow_utf8_names = function() {
  isTRUE(getOption("mlr3.allow_utf8_names"))
}

get_featureless_learner = function(task_type) {
  if (!is.na(task_type)) {
    id = paste0(task_type, ".featureless")
    if (mlr_learners$has(id)) {
      return(mlr_learners$get(id))
    }
  }

  return(NULL)
}

assert_ordered_set = function(x, y, ...) {
  assert_subset(x, y, ...)
  x[reorder_vector(x, y)]
}
