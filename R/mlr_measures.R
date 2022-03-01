#' @title Dictionary of Performance Measures
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [Measure].
#' Each measure has an associated help page, see `mlr_measures_[id]`.
#'
#' This dictionary can get populated with additional measures by add-on packages.
#' E.g., \CRANpkg{mlr3proba} adds survival measures and \CRANpkg{mlr3cluster} adds
#' cluster analysis measures.
#'
#' For a more convenient way to retrieve and construct measures, see [msr()]/[msrs()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict, ..., objects = FALSE)`\cr
#'   [mlr3misc::Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with fields "key", "label", "task_type", "packages",
#'   "predict_type", and "task_properties" as columns.
#'   If `objects` is set to `TRUE`, the constructed objects are returned in the list column named `object`.
#'
#' @family Dictionary
#' @family Measure
#' @seealso
#' Sugar functions: [msr()], [msrs()]
#'
#' Implementation of most measures: \CRANpkg{mlr3measures}
#' @export
#' @examples
#' as.data.table(mlr_measures)
#' mlr_measures$get("classif.ce")
#' msr("regr.mse")
mlr_measures = R6Class("DictionaryMeasure",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @export
as.data.table.DictionaryMeasure = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    m = withCallingHandlers(x$get(key),
      packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, label = m$label, task_type = m$task_type, packages = list(m$packages), predict_type = m$predict_type,
        task_properties = list(m$task_properties)),
      if (objects) list(object = list(m))
    )
  }, .fill = TRUE), "key")[]
}
