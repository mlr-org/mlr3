#' @title Get 'private' part of an R6 Instance
#'
#' @param x ([R6::R6Class]).
#'
#' @return (`environment()`).
#' @noRd
get_private = function(x) {
  x[[".__enclos_env__"]][["private"]]
}

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
