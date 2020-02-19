#' @title Regression Task
#'
#' @include TaskSupervised.R
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for regression problems.
#' The target column is assumed to be numeric.
#' The `task_type` is set to `"regr"`.
#'
#' Predefined tasks are stored in the [mlr3misc::Dictionary] [mlr_tasks].
#'
#' @template rows
#'
#' @family Task
#' @export
#' @examples
#' task = TaskRegr$new("iris", backend = iris, target = "Sepal.Length")
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
    #' Create a new instance.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the task.
    #'
    #' @param backend ([DataBackend])\cr
    #'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
    #'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
    #'
    #' @param target (`character(1)`)\cr
    #'   Name of the target column.
    initialize = function(id, backend, target) {
      assert_string(target)
      super$initialize(id = id, task_type = "regr", backend = backend, target = target)

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
