#' @title Regression task
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for regression problems.
#' The target column is assumed to be numeric.
#'
#' @section Usage:
#' In addition to the interface of [Task]/[TaskSupervised], this class implements:
#' ```
#' t = TaskRegr$new(id, backend, target)
#' ```
#'
#' @section Details:
#' * `$task_type` is `"regr"`.
#'
#' @name TaskRegr
#' @family Task
#' @examples
#' b = as_data_backend(iris)
#' task = TaskRegr$new("iris", backend = b, target = "Sepal.Length")
#' task$task_type
#' task$formula
#' task$truth()
NULL

#' @include TaskSupervised.R
#' @export
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    initialize = function(id, backend, target) {
      super$initialize(id = id, task_type = "regr", backend = backend, target = target)

      assert_string(target) # check for length 1
      # assert_numeric(self$truth(), finite = TRUE, any.missing = FALSE, .var.name = "target column")
      self$measures = list(mlr_measures$get("mse"))
    },

    truth = function(row_ids = NULL) {
      self$data(row_ids, cols = self$target_names)[[1L]]
    }
  )
)
