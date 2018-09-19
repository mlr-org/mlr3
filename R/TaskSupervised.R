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
#'   Subset of row ids to subset rows from the [Backend] using its primary key.
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
      super$initialize(id = id, backend = backend)

      assert_subset(targets, self$feature_names, empty.ok = FALSE)
      self$col_info[id %in% targets, "role" := "target"]

      for (target in targets)
        self$row_info[is.na(target), role := "validation"]
    },

    truth = function(row_ids = NULL) {
      if (is.null(row_ids))
        row_ids = self$row_info[role == "use", "id"][[1L]]
      self$data(row_ids, cols = self$target_names)
    }
  )
)
