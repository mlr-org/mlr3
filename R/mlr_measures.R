#' @title Dictionary of registered performance measures
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Measure
#' @name mlr_measures
#' @examples
#' mlr_measures$ids
#' as.data.table(mlr_measures)
#' m = mlr_measures$get("mmce")
#' print(m)
NULL

#' @include Dictionary.R
DictionaryMeasure = R6Class("DictionaryMeasure",
  inherit = Dictionary,
  cloneable = FALSE,
  public = list(initialize = function() super$initialize("Measure"))
)

#' @export
mlr_measures = NULL

#' @export
as.data.table.DictionaryMeasure = function(x, ...) {
  setkeyv(rbindlist(lapply(x$ids, function(id) {
    m = x$get(id)
    data.table(id = id, task_type = m$task_type, packages = list(m$packages))
  })), "id")[]
}
