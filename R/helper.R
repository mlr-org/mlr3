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

set_data_table_class = function(x, class = NULL) {
  setattr(x, "class", c(class, "data.table", "data.frame"))
}

print_data_table = function(x, hidden_columns) {
  hidden_columns = intersect(names(x), hidden_columns)
  extra_class = class(x)[1]
  set_data_table_class(x)
  print(x[, .SD, .SDcols = !hidden_columns])
  if (length(hidden_columns)) {
    catf(str_indent("Hidden columns:", hidden_columns))
  }
  set_data_table_class(x, extra_class)
}

clone_without = function(x, y) {
  y_prev = x[[y]]
  x[[y]] = NULL
  x2 = x$clone(deep = TRUE)
  x[[y]] = y_prev
  return(x2)
}
