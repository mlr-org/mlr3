#' @title Classification Task
#'
#' @include TaskSupervised.R
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for classification problems.
#' The target column is assumed to be a factor or ordered factor.
#' The `task_type` is set to `"classif"`.
#'
#' Additional task properties include:
#' * `"twoclass"`: The task is a binary classification problem.
#' * `"multiclass"`: The task is a multiclass classification problem.
#'
#' It is recommended to use [as_task_classif()] for construction.
#' Predefined tasks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_tasks].
#'
#' @template param_id
#' @template param_backend
#' @template param_rows
#' @template param_cols
#' @template param_data_format
#'
#' @template seealso_task
#' @export
#' @examples
#' data("Sonar", package = "mlbench")
#' task = as_task_classif(Sonar, target = "Class", positive = "M")
#'
#' task$task_type
#' task$formula()
#' task$truth()
#' task$class_names
#' task$positive
#' task$data(rows = 1:3, cols = task$feature_names[1:2])
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' The function [as_task_classif()] provides an alternative way to construct classification tasks.
    #'
    #' @template param_target
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
    #' True response for specified `row_ids`. Format depends on the task type.
    #' Defaults to all rows with role `"use"`.
    #' @return `factor()`.
    truth = function(rows = NULL) {
      super$truth(rows)[[1L]]
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
      fget(self$col_info, i = self$target_names, j = "levels", key = "id")[[1L]]
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
      lvls = c(positive, negative)
      ii = self$col_info[list(self$target_names), on = "id", which = TRUE]
      set(self$col_info, i = ii, j = "levels", value = list(lvls))
      set(self$col_info, i = ii, j = "fix_factor_levels", value = TRUE)

      lvls
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
      tn = self$target_names
      if (fget(self$col_info, tn, "type", key = "id") %nin% c("factor", "ordered")) {
        stopf("Target column '%s' must be a factor or ordered factor", tn)
      }

      nlvls = length(self$class_names)
      if (nlvls < 2L) {
        stopf("Target column '%s' must have at least two levels", tn)
      }

      private$.properties = setdiff(private$.properties, c("twoclass", "multiclass"))
      private$.properties = union(private$.properties, if (nlvls == 2L) "twoclass" else "multiclass")
    }
  )
)
