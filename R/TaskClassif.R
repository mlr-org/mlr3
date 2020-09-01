#' @title Classification Task
#'
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
#' Predefined tasks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_tasks].
#'
#' @template param_id
#' @template param_backend
#' @template param_rows
#' @template param_cols
#' @template param_data_format
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
#'
#' # possible properties:
#' mlr_reflections$task_properties$classif
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param target (`character(1)`)\cr
    #'   Name of the target column.
    #'
    #' @param positive (`character(1)`)\cr
    #'   Only for binary classification: Name of the positive class.
    #'   The levels of the target columns are reordered accordingly, so that the first element of `$class_names` is the
    #'   positive class, and the second element is the negative class.
    #' @template param_extra_args
    initialize = function(id, backend, target, positive = NULL, extra_args = list()) {
      assert_string(target)
      super$initialize(
        id = id, task_type = "classif", backend = backend,
        target = target, extra_args = extra_args)

      private$.update_class_property()

      if (!is.null(positive)) {
        # NB: this also sets `extra_args$positive`
        self$positive = positive
      }
    },

    #' @description
    #' Calls `$data` from parent class [Task] and ensures that levels of the target column
    #' are in the right order.
    #'
    #' @param ordered (`logical(1)`)\cr
    #'   If `TRUE` (default), data is ordered according to the columns with column role `"order"`.
    #'
    #' @return Depending on the [DataBackend], but usually a [data.table::data.table()].
    data = function(rows = NULL, cols = NULL, data_format = "data.table", ordered = TRUE) {
      data = task_data(self, rows, cols, data_format, ordered)
      fix_factor_levels(data, set_names(list(self$class_names), self$target_names))
    },

    #' @description
    #' True response for specified `row_ids`. Format depends on the task type.
    #' Defaults to all rows with role `"use"`.
    #' @return `factor()`.
    truth = function(rows = NULL) {
      truth = super$truth(rows)[[1L]]
      as_factor(truth, levels = self$class_names)
    },

    #' @description
    #' Updates the cache of stored factor levels, removing all levels not present in the current set of active rows.
    #' `cols` defaults to all columns with storage type "factor" or "ordered".
    #' Also updates the task property `"twoclass"`/`"multiclass"`.
    #'
    #' @return Modified `self`.
    droplevels = function(cols = NULL) {
      super$droplevels()
      private$.update_class_property()
      invisible(self)
    }
  ),

  active = list(
    #' @field class_names (`character()`)\cr
    #' Returns all class labels of the target column.
    class_names = function(rhs) {
      assert_ro_binding(rhs)
      self$col_info[list(self$target_names), "levels", on = "id", with = FALSE][[1L]][[1L]]
    },

    #' @field positive (`character(1)`)\cr
    #' Stores the positive class for binary classification tasks, and `NA` for multiclass tasks.
    #' To switch the positive class, assign a level to this field.
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
      self$extra_args$positive = positive
      self$col_info[list(self$target_names), levels := list(list(c(positive, negative))), on = "id"][]
    },

    #' @field negative (`character(1)`)\cr
    #' Stores the negative class for binary classification tasks, and `NA` for multiclass tasks.
    negative = function(rhs) {
      assert_ro_binding(rhs)
      lvls = self$class_names
      if (length(lvls) != 2L) {
        return(NA_character_)
      }
      return(lvls[2L])
    }
  ),

  private = list(
    .update_class_property = function() {
      nlvls = length(self$class_names)
      if (nlvls < 2L) {
        stopf("Target column '%s' must have at least two levels", self$target_names)
      }

      private$.properties = setdiff(private$.properties, c("twoclass", "multiclass"))
      private$.properties = union(private$.properties, if (nlvls == 2L) "twoclass" else "multiclass")
    }
  )
)
