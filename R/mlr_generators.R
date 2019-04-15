#' @title Dictionary of Task Generators
#'
#' @name mlr_generators
#' @description
#' A simple [Dictionary] storing generator functions returning a [Task].
#'
#' @section Methods:
#' @inheritSection Dictionary Methods
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [Dictionary] -> [data.table::data.table()]\cr
#'   Returns a `data.table()` with fields "key" and "packages" as columns.
#'
#' @family Dictionary
#' @family Task
#' @family Generator
#' @examples
#' g = mlr_generators$get("smiley")
#' task = g$generate(10)
#' print(task)
#' task$data()
NULL

#' @include Dictionary.R
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
