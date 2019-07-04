#' @title Dictionary of Task Generators
#'
#' @name mlr_generators
#' @description
#' A simple [Dictionary] storing generator functions returning a [Task].
#'
#' @section Methods:
#' See [Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with fields "key" and "packages" as columns.
#'
#' @family Dictionary
#' @family Task
#' @family Generator
#' @seealso Example generator [`xor`][mlr_generators_xor].
#' @examples
#' g = mlr_generators$get("smiley")
#' task = g$generate(10)
#' print(task)
#' task$data()
NULL

DictionaryGenerator = R6Class("DictionaryGenerator",
  inherit = Dictionary,
  cloneable = FALSE
)

#' @export
mlr_generators = NULL

#' @export
as.data.table.DictionaryGenerator = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    g = x$get(key)
    list(
      key = key,
      packages = list(g$packages))
  }), "key")[]
}
