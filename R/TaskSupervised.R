#' @title Abstract learning task
#'
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#' It extends [Task] with some handling of target columns.
#'
#' @section Usage:
#' ```
#' t = TaskSupervised$new(id, backend, targets)
#'
#' t$truth(subset = NULL)
#' ```
#'
#' @section Arguments:
#' * `subset` (`vector`):
#'   Subset of row ids to subset rows from the [DataBackend] using its primary key.
#'
#' @section Details:
#' `$truth()` returns the true labels. The type depends on the type of the task.
#'
#' @name TaskSupervised
#' @family Task
#' @keywords internal
NULL

#' @include Task.R
#' @export
TaskSupervised = R6Class("TaskSupervised", inherit = Task,
  public = list(
    initialize = function(id, backend, targets) {
      assert_character(targets, any.missing = FALSE, min.len = 1L)
      super$initialize(id = id, backend = backend)

      i = self$col_info[list(targets), which = TRUE]
      if (anyMissing(i))
        stopf("Target columns %s not in DataBackend", stri_head(targets[is.na(i)]))
      self$col_roles$target = targets
      self$col_roles$feature = setdiff(self$col_roles$feature, targets)

      i = is.na(self$data(rows = self$row_roles$use, cols = targets)[[1L]])
      if (any(i)) {
        self$row_roles$validation = self$row_roles$use[i]
        self$row_roles$use = self$row_roles$use[!i]
      }
    },

    truth = function(row_ids = NULL) {
      self$data(row_ids, cols = self$target_names)
    }
  )
)
