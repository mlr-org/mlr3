#' @title Supervised Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task].
#' @include Task.R
#'
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#' It extends [Task] with methods to handle a target columns.
#'
#' @section Construction:
#' ```
#' t = TaskSupervised$new(id, task_type, backend, target)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Name of the task.
#'
#' * `backend` :: [DataBackend]\cr
#'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
#'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
#'
#' * `task_type` :: `character(1)`\cr
#'   Set in the classes which inherit from this class.
#'   Must be an element of [mlr_reflections$task_types][mlr_reflections].
#'
#' * `target` :: `character(1)`\cr
#'   Name of the target column.
#'
#' @section Fields:
#' @section Methods:
#'
#' @inheritSection Task Fields
#' @inheritSection Task Methods
#'
#' @section Methods:
#' * `truth(row_ids = NULL)` :: [data.table::data.table()]\cr
#'   Table with true  labels for specified `row_ids`.
#'   Defaults to all rows in use.
#'
#'
#' @family Task
#' @keywords internal
#' @export
#' @examples
#' task = TaskSupervised$new("iris", task_type = "classif", backend = iris, target = "Species")
TaskSupervised = R6Class("TaskSupervised", inherit = Task,
  public = list(
    initialize = function(id, task_type, backend, target) {
      super$initialize(id = id, task_type = task_type, backend = backend)
      assert_subset(target, self$col_roles$feature)
      self$col_roles$target = target
      self$col_roles$feature = setdiff(self$col_roles$feature, target)
    },

    truth = function(row_ids = NULL) {
      self$data(row_ids, cols = self$target_names)
    }
  )
)
