#' @title Dictionary of Performance Measures
#'
#' @description
#' A simple [Dictionary] storing objects of class [Measure].
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Measure
#' @name mlr_measures
#' @examples
#' mlr_measures$keys()
#' as.data.table(mlr_measures)
#' mlr_measures$get("mmce")
NULL

#' @include Dictionary.R
DictionaryMeasure = R6Class("DictionaryMeasure",
  inherit = Dictionary,
  cloneable = FALSE
)

#' @export
mlr_measures = DictionaryMeasure$new()

#' @export
as.data.table.DictionaryMeasure = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(id) {
    m = x$get(id)
    list(id = id, task_type = m$task_type, packages = list(m$packages))
  }), "id")[]
}
