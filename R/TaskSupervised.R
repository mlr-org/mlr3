#' @title Supervised Task
#'
#' @include Task.R
#'
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#' It extends [Task] with methods to handle a target columns.
#'
#' @template rows
#'
#' @family Task
#' @keywords internal
#' @export
#' @examples
#' task = TaskSupervised$new("iris", task_type = "classif", backend = iris, target = "Species")
TaskSupervised = R6Class("TaskSupervised", inherit = Task,
  public = list(

    #' @description
    #' Create a new instance.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the task.
    #'
    #' @param task_type (`character(1)`)\cr
    #'   Set in the classes which inherit from this class.
    #'   Must be an element of [mlr_reflections$task_types$type][mlr_reflections].
    #'
    #' @param backend ([DataBackend])\cr
    #'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
    #'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
    #'
    #' @param target (`character(1)`)\cr
    #'   Name of the target column.
    initialize = function(id, task_type, backend, target) {
      super$initialize(id = id, task_type = task_type, backend = backend)
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
