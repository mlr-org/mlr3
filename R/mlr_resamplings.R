#' @title Dictionary of Resampling Strategies
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#' @description
#' A simple [Dictionary] storing objects of class [Resampling].
#' Each resampling has an associated help page, see `mlr_resamplings_[id]`.
#'
#' @section Methods:
#' See [Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with columns `"key"`, `"params"`, and `"iters"`.
#'
#' @family Dictionary
#' @family Resampling
#' @seealso Example resamplings:
#' * [`cv`][mlr_resamplings_cv].
#' * [`bootstrap`][mlr_resamplings_bootstrap].
#' @export
#' @examples
#' as.data.table(mlr_resamplings)
#' mlr_resamplings$get("cv")
mlr_resamplings = R6Class("DictionaryResampling",
  inherit = Dictionary,
  cloneable = FALSE,

  public = list(
    get = function(key, ..., id = NULL, param_vals = NULL) {
      obj = super$get(key, ...)
      if (!is.null(id)) {
        obj$id = id
      }
      if (!is.null(param_vals)) {
        obj$param_set$values = insert_named(obj$param_set$values, param_vals)
      }
      obj
    }
  )
)$new()

#' @export
as.data.table.DictionaryResampling = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    r = x$get(key)
    list(key = key, params = list(r$param_set$ids()), iters = r$iters)
  }), "key")[]
}
