#' @title Get a Default Measure
#'
#' @description
#' Gets the default measures using the information in [mlr_reflections$default_measures][mlr_reflections]:
#' * [`"classif.ce"`][mlr_measures_classif.ce] for classification
#' * [`"regr.mse"`][mlr_measures_regr.mse] for regression
#' * Add-on package may register additional default measures for their own task types
#'
#' @param x :: `character(1)` | [Task] | [Learner]\cr
#'   Get the default measure for the task type `x`, e.g., `"classif"` or `"regr"`.
#'   If you pass an object of type [Task] or [Learner], the corresponding task type is extracted.
#'   If `x` is `NULL`, an empty list is returned.
#' @return list of [Measure].
#' @export
#' @examples
#' default_measures("classif")
#' default_measures("regr")
default_measures = function(x) {
  UseMethod("default_measures")
}

#' @export
default_measures.default = function(x) {
  if (is.null(x))
    return(list())
  assert_choice(x, names(mlr_reflections$default_measures))
  keys = mlr_reflections$default_measures[[x]]
  mlr_measures$mget(keys)
}

#' @export
default_measures.Task = function(x) {
  default_measures.default(x$task_type)
}

#' @export
default_measures.Learner = function(x) {
  default_measures.default(x$task_type)
}
