#' @title Classification task
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for classification problems.
#' The target column is assumed to be a factor.
#'
#' @section Usage:
#' In addition to the interface of [Task]/[TaskSupervised], this class offers:
#' ```
#' # Construction
#' t = TaskClassif$new(id, backend, target, positive = NULL)
#' #
#' t$class_names
#' t$class_n
#' t$task_type
#' ```
#'
#' @section Arguments:
#' * `positive` (`character(1)`):
#'   Name of the "positive" class for binary classification problems.
#'
#' @section Details:
#' `$class_names` returns all class labels of the rows which `role == "use"`.
#' `$class_n` returns the number of class labels of the rows which `role == "use"`.
#' `$task_type` is `"classif"`
#'
#' @name TaskClassif
#' @family Task
#' @examples
#' b = as_data_backend(iris)
#' task = TaskClassif$new("iris", backend = b, target = "Species")
#' task$formula
#' task$truth()
#' task$all_classes
#' task$class_names
#'
#' data("Sonar", package = "mlbench")
#' b = as_data_backend(Sonar)
#' task = TaskClassif$new("sonar", backend = b, target = "Class", positive = "M")
#' task$positive
#' task$negative
NULL


#' @include TaskSupervised.R
#' @export
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    task_type = "classif",
    positive = NA_character_,
    negative = NA_character_,

    initialize = function(id, backend, target, positive = NULL) {
      super$initialize(id = id, backend = backend, target = target)

      info = self$col_info[id == target]
      levels = info$levels[[1L]]

      if (info$type %nin% c("factor", "character"))
        stopf("Target column '%s' must be a factor or character", target)
      if (length(levels) < 2L)
        stopf("Target column '%s' must have at least two levels", target)

      if (length(levels) == 2L) {
        if (is.null(positive)) {
          self$positive = levels[1L]
          info("Setting positive class to '%s'", self$positive)
        } else {
          self$positive = assert_choice(positive, levels)
        }
        self$negative = setdiff(levels, self$positive)
        self$col_info[list(target), levels := list(list(c(self$positive, self$negative))), on = "id"][]
        self$properties = union(self$properties, "twoclass")
      } else {
        if (!is.null(positive))
          stopf("Setting the positive class is only feasible for binary classification")
        self$properties = union(self$properties, "multiclass")
      }

      self$measures = list(mlr_measures$get("mmce"))
    },

    truth = function(row_ids = NULL) {
      factor(self$data(row_ids, cols = self$target_names)[[1L]], levels = self$all_classes)
    }
  ),

  active = list(
    class_names = function() as.character(unique(self$truth())),

    class_n = function() uniqueN(self$truth()),

    all_classes = function() {
      levs = self$col_info[list(self$target_names), levels, on = "id"][[1L]]
    }
  )
)
