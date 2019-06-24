#' @title Dictionary of Tasks
#'
#' @name mlr_tasks
#' @description
#' A simple [Dictionary] storing objects of class [Task].
#' Each task has an associated help page, see `mlr_tasks_[id]`.
#'
#' @section Methods:
#' See [Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [Dictionary] -> [data.table::data.table()]\cr
#'   Returns a `data.table()` with columns `"key"`, `"task_type"`, `"measures"`, `"nrow"`, `"ncol"` and
#'   the number of features of type `"lgl"`, `"int"`, `"dbl"`, `"chr"`, `"fct"` and `"ord"` as columns.
#'
#' @family Dictionary
#' @family Task
#' @seealso Example tasks:
#' * [`iris`][mlr_tasks_iris] (multi-class classification)
#' * [`spam`][mlr_tasks_spam] (binary classification)
#' * [`boston_housing`][mlr_tasks_boston_housing] (regression)
#' @examples
#' as.data.table(mlr_tasks)
#' mlr_tasks$get("iris")
#' head(mlr_tasks$get("iris")$data())
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
NULL

DictionaryTask = R6Class("DictionaryTask",
  inherit = Dictionary,
  cloneable = FALSE
)


#' @export
mlr_tasks = NULL

#' @export
as.data.table.DictionaryTask = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    t = x$get(key)
    feats = translate_types(t$feature_types$type)
    insert_named(list(
      key = key,
      task_type = t$task_type,
      measures = list(ids(t$measures)),
      nrow = t$nrow,
      ncol = t$ncol
    ), table(feats))
  }), "key")[]
}
