#' @include Task.R
TaskSupervised = R6Class("TaskSupervised",
  # Base Class for Supervised Tasks
  inherit = Task,
  public = list(
    default_prediction = NULL,

    initialize = function(id, data, target) {
      super$initialize(id = id, data = data)
      assert_choice(target, self$feature_names)
      self$col_info[id == target, "role" := "target"]

      ii = is.na(self$data(col = target)[[1L]])
      self$row_info[ii, role := "validation"]
    },

    truth = function(rows = NULL) {
      if (is.null(rows))
        rows = self$row_info[role == "training", "id"][[1L]]
      self$data(rows, cols = self$target_names)
    }
  ),

  active = list(
    target_names = function() {
      self$col_info[role == "target", "id"][[1L]]
    },

    # [formula]. target ~ x1 + ... + xp
    formula = function() {
      reformulate(self$feature_names, response = self$target_names)
    }
  )
)
