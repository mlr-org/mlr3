#' @title Dictionary of Resampling Strategies
#'
#' @description
#' A simple [Dictionary] storing objects of class [Resampling].
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @name mlr_resamplings
#' @family Dictionary
#' @family Resampling
#' @examples
#' mlr_resamplings$keys()
#' as.data.table(mlr_resamplings)
#' mlr_resamplings$get("cv")
NULL

#' @include Dictionary.R
DictionaryResampling = R6Class("DictionaryResampling",
  inherit = Dictionary,
  cloneable = FALSE
)

#' @export
mlr_resamplings = DictionaryResampling$new()

#' @export
as.data.table.DictionaryResampling = function(x, ...) {
  map_dtr(x$keys(), function(id) {
    r = x$get(id)
    list(id = id, hyperpars = list(r$param_set$ids), default_iters = r$iters)
  }, .key = "id")
}
