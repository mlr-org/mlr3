#' @title Regression task
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for regression problems.
#' The target column is assumed to be numeric.
#'
#' @section Usage:
#' ```
#' t = TaskRegr$new(id, backend, target)
#' ```
#'
#' @name TaskRegr
#' @family Tasks
#' @examples
#' b = BackendDataTable$new(iris)
#' task = TaskClassif$new("iris", backend = b, target = "Species")
#' task$formula
#' task$class_names
NULL

#' @include TaskSupervised.R
#' @export
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    initialize = function(id, backend, target) {
      super$initialize(id = id, backend = backend, target = target)
      assert_string(target) # check for length 1
      assert_numeric(self$truth()[[1L]], finite = TRUE, any.missing = FALSE, .var.name = "target column")
      self$measures = mlr_measures$mget("mse")
    }
  )
)
