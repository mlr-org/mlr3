#' @title Supervised Task
#'
#' @name TaskSupervised
#' @format [R6Class] object inheriting from [Task].
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#' It extends [Task] with some handling of target columns.
#'
#' @section Usage:
#' Inherits from [Task].
#' ```
#' # Construction
#' t = TaskSupervised$new(id, task_type, backend, target)
#'
#' # Members
#' t$weights
#'
#' # Methods
#' t$truth(row_ids = NULL)
#' ```
#'
#' @section Arguments:
#' * `row_ids` (`integer()` | `character()`):
#'   Subset of row ids to subset rows from the [DataBackend] using its primary key.
#'
#' @section Details:
#' * `$truth()` returns the true labels. The type depends on the type of the task.
#' * `$weights` returns the weights used in the [DataBackend].
#'
#' @family Task
#' @keywords internal
#' @examples
#' b = as_data_backend(iris)
#' task = TaskSupervised$new("iris", task_type = "classif", backend = b, target = "Species")
NULL

#' @include Task.R
#' @export
TaskSupervised = R6Class("TaskSupervised", inherit = Task,
  public = list(
    initialize = function(id, task_type, backend, target) {
      super$initialize(id = id, task_type = task_type, backend = backend)
      assert_subset(target, self$col_roles$feature)
      self$col_roles$target = target
      self$col_roles$feature = setdiff(self$col_roles$feature, target)
    },

    truth = function(row_ids = NULL) {
      self$data(row_ids, cols = self$target_names)
    }
  ),

  active = list(

  )
)
