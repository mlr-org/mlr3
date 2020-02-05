#' @title Dictionary of Resampling Strategies
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [Resampling].
#' Each resampling has an associated help page, see `mlr_resamplings_[id]`.
#'
#' This dictionary can get populated with additional resampling strategies by add-on packages.
#'
#' For a more convenient way to retrieve and construct resampling strategies, see [rsmp()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [mlr3misc::Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with columns `"key"`, `"params"`, and `"iters"`.
#'
#' @family Dictionary
#' @family Resampling
#' @seealso
#' Sugar function: [rsmp()]
#' @export
#' @examples
#' as.data.table(mlr_resamplings)
#' mlr_resamplings$get("cv")
#' rsmp("subsampling")
mlr_resamplings = R6Class("DictionaryResampling",
  inherit = Dictionary,
  cloneable = FALSE,
)$new()

#' @export
as.data.table.DictionaryResampling = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    r = x$get(key)
    list(key = key, params = list(r$param_set$ids()), iters = r$iters)
  }), "key")[]
}
