#' @title Regression Task
#'
#' @name TaskRegr
#' @format [R6Class] object inheriting from [Task]/[TaskSupervised].
#' @description
#' This task specializes [Task] and [TaskSupervised] for regression problems.
#' The target column is assumed to be numeric.
#'
#' @section Usage:
#' Inherits from [Task]/[TaskSupervised].
#' ```
#' t = TaskRegr$new(id, backend, target)
#' ```
#'
#' @section Details:
#' `$task_type` is `"regr"`.
#'
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
      info = self$col_info[id == target]
      if (info$type %nin% c("integer", "numeric"))
        stopf("Target column '%s' must be numeric", target)
      self$measures = list(mlr_measures$get("regr.mse"))
    },

    truth = function(row_ids = NULL) {
      self$data(row_ids, cols = self$target_names)[[1L]]
    }
  )
)
