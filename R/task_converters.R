#' Convert a Task from One Type to Another
#'
#' @description
#' The task's target is replaced by a different column from the data.
#'
#' @param intask ([Task])\cr
#'   A [Task] to be converted.
#' @param target (`character(1)`)\cr
#'   New target to be set, must be a column in the `intask` data.
#'   If `NULL`, no new target is set, and task is converted as-is.
#' @param new_type (`character(1)`)\cr
#'   The new task type. Must be in [`mlr_reflections$task_types`][mlr_reflections]].
#'   If `NULL` (default), a new task with the same task_type is created.
#' @param drop_original_target (`logical(1)`)\cr
#'   If `FALSE` (default), the original target is added as a feature.
#'   Otherwise the original target is dropped.
#' @param drop_levels (`logical(1)`)\cr
#'   If `TRUE` (default), unused levels of the new target variable are dropped.
#'
#' @return [Task] of requested type.
#' @export
convert_task = function(intask, target = NULL, new_type = NULL, drop_original_target = FALSE, drop_levels = TRUE) {
  assert_task(intask)
  target = assert_subset(target, choices = intask$col_info$id) %??% intask$target_names
  new_type = assert_choice(new_type, choices = mlr_reflections$task_types$type, null.ok = TRUE) %??% intask$task_type
  assert_logical(drop_original_target, any.missing = FALSE, len = 1L)

  # get task_type from mlr_reflections and call constructor
  constructor = get(fget(mlr_reflections$task_types, new_type, "task", key = "type")[[1L]])
  common_args = intersect(names(intask$extra_args), names(formals(constructor$public_methods$initialize)))
  newtask = invoke(constructor$new, id = intask$id, backend = intask$backend,
    target = target, .args = intask$extra_args[common_args])
  newtask$extra_args = intask$extra_args

  # copy row_roles / col_roles / properties
  newtask$row_roles = intask$row_roles
  props = intersect(mlr_reflections$task_col_roles[[intask$task_type]], mlr_reflections$task_col_roles[[new_type]])
  newtask$col_roles[props] = intask$col_roles[props]
  newtask$set_col_roles(target, "target")

  # Add the original target(s) as features, only keeping 'new_target'.
  if (!all(intask$target_names == target)) {
    newtask$set_col_roles(setdiff(intask$col_roles$target, target), "feature")
  }

  # during prediction, when target is NA, we do not call droplevels
  if (assert_flag(drop_levels)) {
    newtask$droplevels()
  }

  # if drop_original_target, remove the original target from the features
  if (drop_original_target) {
    newtask$col_roles$feature = setdiff(newtask$col_roles$feature, intask$col_roles$target)
  }

  newtask
}
