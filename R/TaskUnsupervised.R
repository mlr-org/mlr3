#' @title Unsupervised Task
#'
#' @include Task.R
#'
#' @description
#' This is the abstract base class for unsupervised tasks such as cluster tasks in \CRANpkg{mlr3cluster}.
#'
#' @template param_id
#' @template param_task_type
#' @template param_backend
#' @template param_rows
#'
#' @family Task
#' @keywords internal
#' @export
#' @examples
#' library(mlr3cluster)
#' TaskUnsupervised$new("iris", task_type = "clust", backend = iris[, -5])
TaskUnsupervised = R6Class("TaskUnsupervised", inherit = Task,
  public = list(
   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function(id, task_type, backend, extra_args = list()) {
     super$initialize(id = id, task_type = task_type, backend = backend, extra_args = extra_args)
     self$col_roles$target = NULL
     self$col_roles$feature = self$col_roles$feature
   }
  )
)
