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
#' `$truth()` returns (a subset of) the columns which are labeled as target as `data.table`.
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
      # set(self$col_info, i = i, j = "role", "target")
      self$col_roles$target = targets
      self$col_roles$feature = setdiff(self$col_roles$feature, targets)

      for (target in targets)
        self$row_info[is.na(target), role := "validation"]
    },

    truth = function(row_ids = NULL) {
      if (is.null(row_ids))
        row_ids = self$row_roles$use
      self$data(row_ids, cols = self$target_names)
    }
  )
)
