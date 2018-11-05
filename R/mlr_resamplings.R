#' @title Dictionary of Resampling Strategies
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
  setkeyv(rbindlist(lapply(x$keys(), function(id) {
    r = x$get(id)
    data.table(id = id, hyperpars = list(r$par_set$ids), default_iters = r$iters)
  })), "id")[]
}

#' @export
as.data.frame.DictionaryResampling = function(x, ...) {
  setDF(as.data.table.DictionaryResampling(x, ...))[]
}
