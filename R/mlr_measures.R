#' @title Dictionary of Performance Measures
#'
#' @format [R6::R6Class] object.
#' @description
#' A simple [Dictionary] storing objects of class [Measure].
#' Each measure has an associated help page, see `mlr_measures_[id]`.
#'
#' @section Methods:
#' See [Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [Dictionary] -> [data.table::data.table()]\cr
#'   Returns a `data.table()` with fields "key", "task_type", "predict_type",
#'   and "packages" as columns.
#'
#' @family Dictionary
#' @family Measure
#' @name mlr_measures
#' @examples
#' as.data.table(mlr_measures)
#' mlr_measures$get("classif.ce")
NULL

#' @include Dictionary.R
DictionaryMeasure = R6Class("DictionaryMeasure",
  inherit = Dictionary,
  cloneable = FALSE
)

#' @export
mlr_measures = NULL

#' @export
as.data.table.DictionaryMeasure = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    m = x$get(key)
    list(
      key = key,
      task_type = m$task_type,
      packages = list(m$packages),
      predict_type = m$predict_type,
      task_properties = list(m$task_properties)
    )
  }), "key")[]
}
