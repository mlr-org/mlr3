#' @title Classification Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task]/[TaskSupervised].
#' @include TaskSupervised.R
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for classification problems.
#' The target column is assumed to be a factor.
#' Predefined tasks are stored in [mlr_tasks].
#'
#' The `task_type` is set to `"classif"`.
#'
#' @section Construction:
#' ```
#' t = TaskClassif$new(id, backend, target, positive = NULL)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Name of the task.
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
#'
#' @section Fields:
#' * `class_names` :: `character()`\cr
#'   Returns all class labels of the target column.
#'
#' * `class_n` :: `integer(1)`\cr
#'   Returns the number of classes.
#'
#' * `negative` :: `character(1)`\cr
#'   Stores the negative class for binary classification tasks, and `NA` for multiclass tasks.
#'
#' * `positive` :: `character(1)`\cr
#'   Stores the positive class for binary classification tasks, and `NA` for multiclass tasks.
#' @inheritSection Task Fields
#'
#' @section Methods:
#' @inheritSection Task Methods
#'
#' @family Task
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
#' task$negative
#'
#' # possible properties:
#' mlr_reflections$task_properties$classif
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    positive = NULL,
    negative = NULL,

    initialize = function(id, backend, target, positive = NULL) {
      assert_string(target)
      super$initialize(id = id, task_type = "classif", backend = backend, target = target)

      info = self$col_info[id == target]
      levels = info$levels[[1L]]

      if (info$type %nin% c("factor", "character"))
        stopf("Target column '%s' must be a factor or character", target)
      if (length(levels) < 2L)
        stopf("Target column '%s' must have at least two levels", target)

      if (length(levels) == 2L) {
        if (is.null(positive)) {
          self$positive = levels[1L]
          self$negative = levels[2L]
          log_debug("Setting positive class to '%s'", self$positive, namespace = "mlr3")
        } else {
          self$positive = assert_choice(positive, levels)
          self$negative = setdiff(levels, self$positive)
          self$col_info[list(target), levels := list(list(c(self$positive, self$negative))), on = "id"][]
        }

        self$properties = "twoclass"
      } else {
        if (!is.null(positive))
          stopf("Setting the positive class is only feasible for binary classification")
        self$positive = self$negative = NA_character_
        self$properties = "multiclass"
      }

      self$measures = list(mlr_measures$get("classif.mmce"))
    },

    truth = function(row_ids = NULL) {
      res = self$data(row_ids, cols = self$target_names)[[1L]]
      factor(res, levels = self$class_names)
    }
  ),

  active = list(
    class_names = function() {
      self$col_info[list(self$target_names), "levels", with = FALSE][[1L]][[1L]]
    },

    class_n = function() length(self$class_names)
  )
)
