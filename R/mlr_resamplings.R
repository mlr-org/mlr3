#' @title Dictionary of Resampling Strategies
#'
#' @format [R6::R6Class] object.
#'
#' @description
#' A simple [Dictionary] storing objects of class [Resampling].
#' Each resampling has an associated help page, see `mlr_resamplings_[id]`.
#'
#' @section Methods:
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [Dictionary] -> [data.table::data.table()]\cr
#'   Returns a `data.table()` with fields "key", "hyperpars", and "iters"
#'   as columns.
#'
#' @inheritSection Dictionary Methods
#'
#' @name mlr_resamplings
#' @family Dictionary
#' @family Resampling
#' @examples
#' as.data.table(mlr_resamplings)
#' mlr_resamplings$get("cv")
NULL

#' @include Dictionary.R
DictionaryResampling = R6Class("DictionaryResampling",
  inherit = Dictionary,
  cloneable = FALSE,

  public = list(
    get = function(key, id = NULL, param_vals = NULL) {
      obj = dictionary_retrieve(self, key)
      if (!is.null(id))
        obj$id = id
      if (!is.null(param_vals))
        obj$param_set$values = insert_named(obj$param_set$values, param_vals)
      obj
    }
  )
)

#' @export
mlr_resamplings = DictionaryResampling$new()

#' @export
as.data.table.DictionaryResampling = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    r = x$get(key)
    list(key = key, hyperpars = list(r$param_set$ids()), iters = r$iters)
  }), "key")[]
}
