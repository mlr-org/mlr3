#' @title Get a Default Measure
#'
#' @description
#' Gets the default measures using the information in [mlr_reflections$default_measures][mlr_reflections]:
#' * [`"classif.ce"`][mlr_measures_classif.ce] for classification (`"classif"`).
#' * [`"regr.mse"`][mlr_measures_regr.mse] for regression (`"regr"`).
#' * Add-on package may register additional default measures for their own task types.
#'
#' @param task_type :: `character(1)`\cr
#'   Get the default measure for the task type `task_type`, e.g., `"classif"` or `"regr"`.
#'   If `task_type` is `NULL`, an empty list is returned.
#' @return list of [Measure].
#' @export
#' @examples
#' default_measures("classif")
#' default_measures("regr")
#' @export
default_measures = function(task_type) {
  if (is.null(task_type))
    return(list())
  assert_choice(task_type, names(mlr_reflections$default_measures))
  keys = mlr_reflections$default_measures[[task_type]]
  mlr_measures$mget(keys)
}
