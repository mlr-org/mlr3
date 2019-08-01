#' @title Dictionary of Task Generators
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
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
#' @family Generator
#' @seealso
#' Example generators: [`xor`][mlr_generators_xor]
#' @export
#' @examples
#' g = mlr_generators$get("smiley")
#' task = g$generate(10)
#' print(task)
#' task$data()
mlr_generators = DictionaryGenerator = R6Class("DictionaryGenerator",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @export
as.data.table.DictionaryGenerator = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    g = x$get(key)
    list(
      key = key,
      packages = list(g$packages))
  }), "key")[]
}
