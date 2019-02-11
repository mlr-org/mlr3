#' @title Dictionary of Resampling Strategies
#'
#' @format [R6::R6Class] object.
#' @description
#' A simple [Dictionary] storing objects of class [Resampling].
#' Each resampling has an associated help page, see `mlr_resamplings_[id]`.
#'
#' @section Usage:
#' Inherits from [Dictionary].
#'
#' @name mlr_resamplings
#' @family Dictionary
#' @family Resampling
#' @examples
#' mlr_resamplings$ids()
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
  setkeyv(map_dtr(x$ids(), function(id) {
    r = x$get(id)
    list(id = id, hyperpars = list(r$param_set$ids), default_iters = r$iters)
  }), "id")[]
}
