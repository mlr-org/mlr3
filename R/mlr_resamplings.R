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
#' For a more convenient way to retrieve and construct resampling strategies, see [rsmp()]/[rsmps()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict, ..., extract = NULL)`\cr
#'   [mlr3misc::Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with columns "key", "params", and "iters".
#'   Additional columns can be extracted with function `extract` which has to return a named list which
#'   is passed to [data.table::rbindlist()] to construct additional columns.
#'
#' @family Dictionary
#' @family Resampling
#' @seealso
#' Sugar functions: [rsmp()], [rsmps()]
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
as.data.table.DictionaryResampling = function(x, ..., extract = NULL) { # nolint
  if (is.null(extract)) {
    extract = function(x) NULL
  } else {
    assert_function(extract)
  }

  setkeyv(map_dtr(x$keys(), function(key) {
    r = tryCatch(x$get(key),
      missingDefaultError = function(e) NULL)
    if (is.null(r)) {
      return(list(key = key))
    }

    insert_named(
      list(key = key, params = list(r$param_set$ids()), iters = r$iters),
      extract(r)
    )
  }, .fill = TRUE), "key")[]
}
