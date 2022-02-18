#' @title Unsupervised Task
#'
#' @include Task.R
#'
#' @description
#' This class defines an unsupervised machine learning problem on which [Learner]s operate on.
#'
#' @details
#' Supported unsupervised tasks in mlr3 include clustering ([TaskClust]) via the extension
#' \CRANpkg{mlr3cluster} and density estimation via (\CRANpkg{mlr3proba}). When working with
#' tasks use these subclasses.
#' You can learn more about tasks in the chapters "Basics" and "Special Tasks" in the
#' [mlr3 book](https://mlr3book.mlr-org.com/basics.html).
#'
#' @section Development:
#' This is the abstract base class for unsupervised tasks.
#' You can learn more about tasks in the chapters "Basics" and "Special Tasks" in the
#' [mlr3 book](https://mlr3book.mlr-org.com/basics.html).
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
