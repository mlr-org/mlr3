#' @title Classification Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task]/[TaskSupervised].
#' @include TaskSupervised.R
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for classification problems.
#' The target column is assumed to be a factor.
#' The `task_type` is set to `"classif"`.
#'
#' Additional task properties include:
#' * `"twoclass"`: The task is a binary classification problem.
#' * `"multiclass"`: The task is a multiclass classification problem.
#'
#' Predefined tasks are stored in the [mlr3misc::Dictionary] [mlr_tasks].
#'
#' @section Construction:
#' ```
#' t = TaskClassif$new(id, backend, target, positive = NULL)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the task.
#'
#' * `backend` :: [DataBackend]\cr
#'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
#'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
#'
#' * `target` :: `character(1)`\cr
#'   Name of the target column.
#'
#' * `positive` :: `character(1)`\cr
#'   Only for binary classification: Name of the positive class.
#'   The levels of the target columns are reordered accordingly, so that the first element of `$class_names` is the
#'   positive class, and the second element is the negative class.
#'
#' @section Fields:
#' All methods from [TaskSupervised], and additionally:
#'
#' * `class_names` :: `character()`\cr
#'   Returns all class labels of the target column.
#'
#' * `positive` :: `character(1)`\cr
#'   Stores the positive class for binary classification tasks, and `NA` for multiclass tasks.
#'   To switch the positive class, assign a level to this field.
#'
#' * `negative` :: `character(1)`\cr
#'   Stores the negative class for binary classification tasks, and `NA` for multiclass tasks.
#'
#' @section Methods:
#' See [TaskSupervised].
#'
#' @family Task
#' @seealso
#' Example classification tasks: [`iris`][mlr_tasks_iris]
#' @export
#' @examples
#' data("Sonar", package = "mlbench")
#' task = TaskClassif$new("sonar", backend = Sonar, target = "Class", positive = "M")
#'
#' task$task_type
#' task$formula()
#' task$truth()
#' task$class_names
#' task$positive
#'
#' # possible properties:
#' mlr_reflections$task_properties$classif
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    initialize = function(id, backend, target, positive = NULL) {

      assert_string(target)
      super$initialize(id = id, task_type = "classif", backend = backend, target = target)

      info = self$col_info[id == target]
      levels = info$levels[[1L]]

      if (info$type != "factor") {
        stopf("Target column '%s' must be a factor", target)
      }
      if (length(levels) < 2L) {
        stopf("Target column '%s' must have at least two levels", target)
      }

      self$properties = union(self$properties, if (length(levels) == 2L) "twoclass" else "multiclass")
      if (!is.null(positive)) {
        self$positive = positive
      }
    },

    truth = function(rows = NULL) {
      truth = super$truth(rows)[[1L]]
      as_factor(truth, levels = self$class_names)
    }
  ),

  active = list(
    class_names = function(rhs) {
      assert_ro_binding(rhs)
      self$col_info[list(self$target_names), "levels", on = "id", with = FALSE][[1L]][[1L]]
    },

    positive = function(rhs) {
      lvls = self$class_names
      if (missing(rhs)) {
        if (length(lvls) != 2L) {
          return(NA_character_)
        }
        return(lvls[1L])
      }

      if (length(lvls) != 2L) {
        stopf("Setting the positive class is only feasible for binary classification")
      }
      positive = assert_choice(rhs, lvls)
      negative = setdiff(lvls, rhs)
      self$col_info[list(self$target_names), levels := list(list(c(positive, negative))), on = "id"][]
    },

    negative = function(rhs) {
      assert_ro_binding(rhs)
      lvls = self$class_names
      if (length(lvls) != 2L) {
        return(NA_character_)
      }
      return(lvls[2L])
    }
  )
)
