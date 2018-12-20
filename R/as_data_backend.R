#' @title Wrap a backend around data.
#' @description
#' Wraps a [DataBackend] around data.
#'
#' @section Arguments:
#' * `data` ([data.frame] / [tibble::tibble] / [data.table::data.table]):\cr
#'   Data to wrap the backend around. Typically a `data.frame`.
#'
#' @section Details:
#'   See `methods("as_data_backend")` for possible input formats.
#' @param data \cr
#'   Data to wrap the backend around. Typically a `data.frame`.
#' @param ... \cr
#'   Additional arguments passed to the respective [DataBackend] method.
#' @return [DataBackend]
#' @family DataBackend
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/as_data_backend.html)
#' @export
as_data_backend = function(data, ...) {
  UseMethod("as_data_backend")
}
