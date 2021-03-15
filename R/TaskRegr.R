#' @title Regression Task
#'
#' @include TaskSupervised.R
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for regression problems.
#' The target column is assumed to be numeric.
#' The `task_type` is set to `"regr"`.
#'
#' Predefined tasks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_tasks].
#'
#' @template param_rows
#' @template param_id
#' @template param_backend
#'
#' @template seealso_task
#' @export
#' @examples
#' task = as_task_regr(palmerpenguins::penguins, target = "bill_length_mm")
#' task$task_type
#' task$formula()
#' task$truth()
#' task$data(rows = 1:3, cols = task$feature_names[1:2])
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' The function [as_task_regr()] provides an alternative way to construct regression tasks.
    #'
    #' @template param_target
    #' @template param_extra_args
    initialize = function(id, backend, target, extra_args = list()) {
      assert_string(target)
      super$initialize(
        id = id, task_type = "regr", backend = backend,
        target = target, extra_args = extra_args)

      type = self$col_info[id == target]$type
      if (type %nin% c("integer", "numeric")) {
        stopf("Target column '%s' must be numeric", target)
      }
    },

    #' @description
    #' True response for specified `row_ids`. Format depends on the task type.
    #' Defaults to all rows with role "use".
    #' @return `numeric()`.
    truth = function(rows = NULL) {
      super$truth(rows)[[1L]]
    }
  )
)
