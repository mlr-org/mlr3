#' @title Supervised Task
#'
#' @include Task.R
#'
#' @description
#' A TaskSupervised defines a supervised machine learning problem, meaning a learning problem
#' with a target variable such as classification, regression, or Survival Analysis (\CRANpkg{mlr3proba}).
#' You can learn more about tasks in the chapters "Basics" and "Special Tasks" in the
#' [mlr3 book](https://mlr3book.mlr-org.com/basics.html).
#'
#' @details
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#' It extends [Task] with methods to handle a target columns.
#' Supervised tasks for probabilistic regression (including survival analysis) can be
#' found in \CRANpkg{mlr3proba}.
#'
#' @template param_id
#' @template param_task_type
#' @template param_backend
#' @template param_rows
#' @template param_extra_args
#'
#' @template seealso_task
#' @keywords internal
#' @export
#' @examples
#' TaskSupervised$new("penguins", task_type = "classif", backend = palmerpenguins::penguins,
#'   target = "species")
TaskSupervised = R6Class("TaskSupervised", inherit = Task,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param target (`character(1)`)\cr
    #'   Name of the target column.
    initialize = function(id, task_type, backend, target, extra_args = list()) {
      super$initialize(id = id, task_type = task_type, backend = backend, extra_args = extra_args)
      assert_subset(target, self$col_roles$feature)
      self$col_roles$target = target
      self$col_roles$feature = setdiff(self$col_roles$feature, target)
    },

    #' @description
    #' True response for specified `row_ids`. Format depends on the task type.
    #' Defaults to all rows with role "use".
    truth = function(rows = NULL) {
      self$data(rows, cols = self$target_names)
    }
  )
)
