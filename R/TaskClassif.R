#' @title Classification task
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for classification problems.
#' The target column is assumed to be a factor.
#'
#' @section Usage:
#' ```
#' t = TaskClassif$new(id, backend, target, positive = NULL)
#'
#' t$class_names
#' t$class_n
#' t$type
#' ```
#'
#' @section Arguments:
#' * `positive` (`character(1)`):
#'   Name of the "positive" class for binary classification problems.
#'
#' @section Details:
#' `$class_names` returns all class labels of the rows which `role == "use"`.
#' `$class_n` returns the number of class labels of the rows which `role == "use"`.
#' `$type` is `"classif"`
#'
#' @name TaskClassif
#' @family Task
#' @examples
#' b = DataBackendDataTable$new(iris)
#' task = TaskClassif$new("iris", backend = b, target = "Species")
#' task$formula
#' task$class_names
NULL


#' @include TaskSupervised.R
#' @export
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    type = "classif",
    positive = NA_character_,

    initialize = function(id, backend, target, positive = NULL) {
      super$initialize(id = id, backend = backend, target = target)

      # FIXME: use col_info$levels here
      truth = factor(self$truth()[[1L]])
      assert_factor(truth, min.levels = 2L, any.missing = FALSE, empty.levels.ok = FALSE, .var.name = "target column")

      if (is.null(positive)) {
        if (nlevels(truth) == 2L) {
          self$positive = levels(truth)[1L]
          info("Setting positive class to '%s'", self$positive)
        }
      } else {
        if (nlevels(truth) != 2L)
          stopf("Setting the positive class is only feasible for binary classification")
        self$positive = positive
      }

      self$measures = list(mlr_measures$get("mmce"))
    }
  ),

  active = list(
    class_names = function() as.character(unique(self$truth()[[1L]])),
    class_n = function() uniqueN(self$truth()[[1L]]),
    all_classes = function() as.character(unique(self$backend$data(self$row_info$id, self$target_names)[[1L]]))
  )
)
