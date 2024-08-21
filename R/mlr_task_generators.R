#' @title Dictionary of Task Generators
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [TaskGenerator].
#' Each task generator has an associated help page, see `mlr_task_generators_[id]`.
#'
#' This dictionary can get populated with additional task generators by add-on packages.
#'
#' For a more convenient way to retrieve and construct task generators, see [tgen()]/[tgens()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict, ..., objects = FALSE)`\cr
#'   [mlr3misc::Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with fields "key", "label", "task_type", "params", and "packages" as columns.
#'   If `objects` is set to `TRUE`, the constructed objects are returned in the list column named `object`.
#'
#' @family Dictionary
#' @family TaskGenerator
#' @seealso
#' Sugar functions: [tgen()], [tgens()]
#' @export
#' @examples
#' mlr_task_generators$get("smiley")
#' tgen("2dnormals")
mlr_task_generators = R6Class("DictionaryTaskGenerator",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @export
as.data.table.DictionaryTaskGenerator = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    g = withCallingHandlers(x$get(key),
      packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, label = g$label, task_type = g$task_type, params = list(g$param_set$ids()), packages = list(g$packages)),
      if (objects) list(object = list(g))
    )
  }, .fill = TRUE), "key")[]
}
