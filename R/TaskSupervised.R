#' @title Supervised Task
#' @format [R6Class] object
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#' It extends [Task] with some handling of target columns.
#'
#' @section Usage:
#' Inherits from [Task]
#'
#' ```
#' # Construction
#' t = TaskSupervised$new(id, task_type, backend, targets)
#'
#' # Members
#' t$weights
#'
#' # Methods
#' t$truth(subset = NULL)
#' ```
#'
#' @section Arguments:
#' * `subset` (`integer()` | `character()`):\cr
#'   Subset of row ids to subset rows from the [DataBackend] using its primary key.
#'
#' @section Details:
#' * `$weights` returns the weights used in the [DataBackend].
#' * `$truth()` returns the true labels. The type depends on the type of the task.
#'
#' @name TaskSupervised
#' @family Task
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/TaskSupervised.html)
#' @examples
#' b = as_data_backend(iris)
#' task = TaskSupervised$new("iris", task_type = "classif", backend = b, targets = "Species")
#'
#' @keywords internal
NULL

#' @include Task.R
#' @export
TaskSupervised = R6Class("TaskSupervised", inherit = Task,
  public = list(
    initialize = function(id, task_type, backend, targets) {
      assert_character(targets, any.missing = FALSE, min.len = 1L)
      super$initialize(id = id, task_type = task_type, backend = backend)

      i = which(targets %nin% self$col_roles$feature)
      if (length(i))
        stopf("Target columns %s not in DataBackend", str_collapse(targets[i], quote = "'"))

      self$col_roles$target = targets
      self$col_roles$feature = setdiff(self$col_roles$feature, targets)
    },

    truth = function(row_ids = NULL) {
      self$data(row_ids, cols = self$target_names)
    }
  )
)
