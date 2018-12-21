#' @title Dictionary of Performance Measures
#'
#' @format [R6Class] object
#' @description
#' A simple [Dictionary] storing objects of class [Measure].
#' Each measure has an associated help page, see `mlr_measures_[id]`.
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Measure
#' @name mlr_measures
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/mlr_measures.html)
#' @examples
#' mlr_measures$ids()
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
  setkeyv(map_dtr(x$ids(), function(id) {
    m = x$get(id)
    list(id = id, task_type = m$task_type, packages = list(m$packages))
  }), "id")[]
}
