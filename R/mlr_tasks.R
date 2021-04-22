#' @title Dictionary of Tasks
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [Task].
#' Each task has an associated help page, see `mlr_tasks_[id]`.
#'
#' This dictionary can get populated with additional tasks by add-on packages,
#' e.g. \CRANpkg{mlr3data}, \CRANpkg{mlr3proba} or \CRANpkg{mlr3cluster}.
#' \CRANpkg{mlr3oml} allows to interact with [OpenML](https://www.openml.org).
#'
#' For a more convenient way to retrieve and construct tasks, see [tsk()]/[tsks()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [mlr3misc::Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with columns `"key"`, `"task_type"`, `"measures"`, `"nrow"`, `"ncol"` and
#'   the number of features of type `"lgl"`, `"int"`, `"dbl"`, `"chr"`, `"fct"` and `"ord"` as columns.
#'
#' @family Dictionary
#' @family Task
#' @seealso
#' Sugar functions: [tsk()], [tsks()]
#'
#' Extension Packages: \CRANpkg{mlr3data}
#' @export
#' @examples
#' as.data.table(mlr_tasks)
#' task = mlr_tasks$get("penguins") # same as tsk("penguins")
#' head(task$data())
#'
#' # Add a new task, based on a subset of penguins:
#' data = palmerpenguins::penguins
#' data$species = factor(ifelse(data$species == "Adelie", "1", "0"))
#' task = TaskClassif$new("penguins.binary", data, target = "species", positive = "1")
#'
#' # add to dictionary
#' mlr_tasks$add("penguins.binary", task)
#'
#' # list available tasks
#' mlr_tasks$keys()
#'
#' # retrieve from dictionary
#' mlr_tasks$get("penguins.binary")
#'
#' # remove task again
#' mlr_tasks$remove("penguins.binary")
mlr_tasks = R6Class("DictionaryTask",
  inherit = Dictionary,
  cloneable = FALSE
)$new()


#' @export
as.data.table.DictionaryTask = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    t = tryCatch(x$get(key),
      missingDefaultError = function(e) NULL)
    if (is.null(t)) {
      return(list(key = key))
    }

    feats = translate_types(t$feature_types$type)
    insert_named(list(
      key = key,
      task_type = t$task_type,
      nrow = t$nrow,
      ncol = t$ncol,
      properties = list(t$properties)
    ), table(feats))
  }, .fill = TRUE), "key")[]
}
