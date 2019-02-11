#' @title Dictionary of Performance Measures
#'
#' @format [R6::R6Class] object.
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
#' @examples
#' as.data.table(mlr_measures)
#' mlr_measures$get("classif.mmce")
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
  setkeyv(map_dtr(x$keys(), function(key) {
    m = x$get(key)
    list(id = key,
      task_type = m$task_type,
      predict_type = m$predict_type,
      packages = list(m$packages))
  }), "id")[]
}
