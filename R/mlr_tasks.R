#' @title Dictionary of Tasks
#'
#' @name mlr_tasks
#' @description
#' A simple [Dictionary] storing objects of class [Task].
#' Each task has an associated help page, see `mlr_tasks_[id]`.
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Task
#' @examples
#' mlr_tasks$ids()
#' as.data.table(mlr_tasks)
#' mlr_tasks$get("iris")
#' head(mlr_tasks$get("iris")$data())
#'
#' # Add a new task, based on a subset of iris:
#' data = iris
#' data$Species = factor(ifelse(data$Species == "setosa", "1", "0"))
#' b = as_data_backend(data)
#' task = TaskClassif$new("iris.binary", b, target = "Species", positive = "1")
#'
#' # add to dictionary
#' mlr_tasks$add("iris.binary", task)
#'
#' # list available tasks
#' mlr_tasks$ids()
#'
#' # retrieve from dictionary
#' mlr_tasks$get("iris.binary")
#'
#' # remove task again
#' mlr_tasks$remove("iris.binary")
NULL

#' @include Dictionary.R
DictionaryTask = R6Class("DictionaryTask",
  inherit = Dictionary,
  cloneable = FALSE
)


#' @export
mlr_tasks = DictionaryTask$new()

#' @export
as.data.table.DictionaryTask = function(x, ...) {
  setkeyv(map_dtr(x$ids(), function(id) {
    t = x$get(id)
    feats = factor(t$feature_types$type, levels = mlr_reflections$task_feature_types)
    feats = as.list(table(feats))
    insert_named(data.table(id = id, type = t$task_type, measures = list(ids(t$measures)),
        nrow = t$nrow, ncol = t$ncol), feats)
  }), "id")[]
}
