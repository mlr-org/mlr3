#' @title Dictionary of registered performance measures
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
#' m = mlr_measures$get("mmce")
#' print(m)
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
