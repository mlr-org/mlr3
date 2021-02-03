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
#' More example tasks can be found in this dictionary after loading \CRANpkg{mlr3data}.
#'
#' @template param_rows
#' @template param_id
#' @template param_backend
#'
#' @family Task
#' @export
#' @examples
#' task = TaskRegr$new("penguins", backend = palmerpenguins::penguins, target = "bill_length_mm")
#' task$task_type
#' task$formula()
#' task$truth()
#'
#' # possible properties:
#' mlr_reflections$task_properties$regr
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' The function [new_task_regr()] wraps this constructor.
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

#' @title Create a New Regression Task
#'
#' @description
#' A wrapper around the constructor of [TaskRegr].
#'
#' @template param_backend
#' @template param_target
#' @param id (`character(1)`)\cr
#'   Identifier for the new task.
#'   Defaults to the (deparsed and substituted) name of `backend`.
#' @export
#' @examples
#' task = new_task_regr(mtcars, target = "mpg")
new_task_regr = function(backend, target, id = deparse(substitute(backend))) {
  TaskRegr$new(id = id, backend = backend, target = target)
}
