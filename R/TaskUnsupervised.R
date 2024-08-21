#' @title Unsupervised Task
#'
#' @include Task.R
#'
#' @description
#' This is the abstract base class for unsupervised tasks such as cluster tasks in \CRANpkg{mlr3cluster} and \CRANpkg{mlr3spatial}.
#'
#' @template param_id
#' @template param_task_type
#' @template param_backend
#' @template param_label
#' @template param_extra_args
#'
#' @template seealso_task
#' @keywords internal
#' @export
#' @examples
#' TaskUnsupervised$new("penguins", task_type = "regr", backend = palmerpenguins::penguins)
TaskUnsupervised = R6Class("TaskUnsupervised",
  inherit = Task,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, task_type = "unsupervised", backend, label = NA_character_, extra_args = list()) {
      super$initialize(id = id, task_type = task_type, backend = backend, label = label, extra_args = extra_args)
    }
  )
)
