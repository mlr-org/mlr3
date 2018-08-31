#' @include Task.R
#' @examples
#' b = BackendDataTable$new(iris)
#' task = TaskSupervised$new("iris", backend = b, target = "Species")
#' task$target_names
#' task$formula
TaskSupervised = R6Class("TaskSupervised",
  # Base Class for Supervised Tasks
  inherit = Task,
  public = list(
    initialize = function(id, backend, target) {
      super$initialize(id = id, backend = backend)

      assert_choice(target, self$feature_names)
      self$col_info[id == target, "role" := "target"]

      ii = is.na(self$data(col = target)[[1L]])
      self$row_info[ii, role := "validation"]
    },

    truth = function(row_ids = NULL) {
      if (is.null(row_ids))
        row_ids = self$row_info[role == "training", "id"][[1L]]
      self$data(row_ids, cols = self$target_names)
    }
  )
)
