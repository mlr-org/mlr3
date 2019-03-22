#' @title Regression Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task]/[TaskSupervised].
#' @include TaskSupervised.R
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for regression problems.
#' The target column is assumed to be numeric.
#' Predefined tasks are stored in [mlr_tasks].
#'
#' The `task_type` is set to `"classif"`.
#'
#' @section Construction:
#' ```
#' t = TaskRegr$new(id, backend, target)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Name of the task.
#'
#' * `backend` :: ([DataBackend] | `data.frame()` | ...)\cr
#'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
#'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
#'
#' * `target` :: `character(1)`\cr
#'   Name of the target column.
#'
#' @section Fields:
#' @inheritSection Task Fields
#'
#' @section Methods:
#' @inheritSection Task Methods
#'
#' @family Task
#' @export
#' @examples
#' task = TaskRegr$new("iris", backend = iris, target = "Sepal.Length")
#' task$task_type
#' task$formula()
#' task$truth()
#'
#' # possible properties:
#' mlr_reflections$task_properties$regr
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    initialize = function(id, backend, target) {
      assert_string(target)
      super$initialize(id = id, task_type = "regr", backend = backend, target = target)

      type = self$col_info[id == target]$type
      if (type %nin% c("integer", "numeric"))
        stopf("Target column '%s' must be numeric", target)
      self$measures = list(mlr_measures$get("regr.mse"))
    },

    truth = function(row_ids = NULL) {
      self$data(row_ids, cols = self$target_names)[[1L]]
    }
  )
)
