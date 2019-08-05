#' @title Dictionary of Performance Measures
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
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
#'   Returns a [data.table::data.table()] with fields "key", "task_type", "predict_type",
#'   and "packages" as columns.
#'
#' @family Dictionary
#' @family Measure
#' @seealso
#' Example measures: [`classif.auc`][mlr_measures_classif.auc], [`time_train`][mlr_measures_time_train]
#' @export
#' @examples
#' as.data.table(mlr_measures)
#' mlr_measures$get("classif.ce")

mlr_measures = R6Class("DictionaryMeasure",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

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
