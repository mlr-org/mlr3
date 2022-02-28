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
#' * `as.data.table(dict, ..., extract = NULL)`\cr
#'   [mlr3misc::Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with fields "key", "task_type", "params", and "packages" as columns.
#'   Additional columns can be extracted with function `extract` which has to return a named list which
#'   is passed to [data.table::rbindlist()] to construct additional columns.
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
as.data.table.DictionaryTaskGenerator = function(x, ..., extract = NULL) {
  if (is.null(extract)) {
    extract = function(x) NULL
  } else {
    assert_function(extract)
  }

  setkeyv(map_dtr(x$keys(), function(key) {
    g = withCallingHandlers(x$get(key),
      packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, task_type = g$task_type, params = list(g$param_set$ids()), packages = list(g$packages)),
      extract(g)
    )
  }, .fill = TRUE), "key")[]
}
