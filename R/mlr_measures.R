#' @title Dictionary of Performance Measures
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
  setkeyv(rbindlist(lapply(x$keys(), function(id) {
    m = x$get(id)
    data.table(id = id, task_type = m$task_type, packages = list(m$packages))
  })), "id")[]
}

#' @export
as.data.frame.DictionaryMeasure = function(x, ...) {
  setDF(as.data.table.DictionaryMeasure(x, ...))[]
}
