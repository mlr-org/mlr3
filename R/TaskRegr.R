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



#' @title Convert to a Regression Task
#' @param x (`any`)\cr
#'   Object to convert, e.g. a `data.frame()`.
#' @param ... (`any`)\cr
#'   Additional arguments.
#' @export
as_task_regr = function(x, ...) {
  UseMethod("as_task_regr")
}


#' @rdname as_task_regr
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`, e.g.
#'   by cloning it or constructing it from a [dictionary][mlr3misc::Dictionary] such as [mlr_learners].
#' @export
as_task_regr.TaskRegr = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}


#' @rdname as_task_regr
#' @template param_target
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of `x`.
#' @export
as_task_regr.data.frame = function(x, target, id = deparse(substitute(x)), ...) { # nolint
  TaskRegr$new(id = id, backend = x, target = target)
}

#' @rdname as_task_regr
#' @export
as_task_regr.DataBackend = function(x, target, id = deparse(substitute(x)), ...) { # nolint
  TaskRegr$new(id = id, backend = x, target = target)
}
