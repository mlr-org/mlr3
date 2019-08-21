#' @title Dictionary of Tasks
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#' @description
#' A simple [Dictionary] storing objects of class [Task].
#' Each task has an associated help page, see `mlr_tasks_[id]`.
#'
#' This dictionary can get populated with additional tasks by add-on packages.
#'
#' For a more convenient way to retrieve and construct tasks, see [tsk()].
#'
#' @section Methods:
#' See [Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with columns `"key"`, `"task_type"`, `"measures"`, `"nrow"`, `"ncol"` and
#'   the number of features of type `"lgl"`, `"int"`, `"dbl"`, `"chr"`, `"fct"` and `"ord"` as columns.
#'
#' @family Dictionary
#' @family Task
#' @seealso
#' Example tasks: [`iris`][mlr_tasks_iris] (multi-class classification), [`spam`][mlr_tasks_spam] (binary classification), [`boston_housing`][mlr_tasks_boston_housing] (regression)
#'
#' Sugar function: [tsk()]
#' @export
#' @examples
#' as.data.table(mlr_tasks)
#' task = mlr_tasks$get("iris") # same as tsk("iris")
#' head(task$data())
#'
#' # Add a new task, based on a subset of iris:
#' data = iris
#' data$Species = factor(ifelse(data$Species == "setosa", "1", "0"))
#' task = TaskClassif$new("iris.binary", data, target = "Species", positive = "1")
#'
#' # add to dictionary
#' mlr_tasks$add("iris.binary", task)
#'
#' # list available tasks
#' mlr_tasks$keys()
#'
#' # retrieve from dictionary
#' mlr_tasks$get("iris.binary")
#'
#' # remove task again
#' mlr_tasks$remove("iris.binary")
mlr_tasks = R6Class("DictionaryTask",
  inherit = Dictionary,
  cloneable = FALSE
)$new()


#' @export
as.data.table.DictionaryTask = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    t = x$get(key)
    feats = translate_types(t$feature_types$type)
    insert_named(list(
      key = key,
      task_type = t$task_type,
      nrow = t$nrow,
      ncol = t$ncol
    ), table(feats))
  }), "key")[]
}
