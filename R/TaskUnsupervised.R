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
#' @template seealso_task
#' @keywords internal
#' @export
#' @examples
#' TaskUnsupervised$new("penguins", task_type = "regr", backend = palmerpenguins::penguins)
TaskUnsupervised = R6Class("TaskUnsupervised", inherit = Task)
