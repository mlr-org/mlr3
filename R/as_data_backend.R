#' @title Create a Data Backend.
#'
#' @description
#' Wraps a [DataBackend] around data.
#'
#' @param data Data to wrap the backend around. For a `data.frame()` (this includes
#'   tibbles and data.tables), create a [DataBackendDataTable].
#'   See `methods("as_data_backend")` for possible input formats.
#' @param ... Additional arguments passed to the respective [DataBackend] method.
#'
#' @return [DataBackend]
#' @family DataBackend
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/as_data_backend.html)
#' @export
as_data_backend = function(data, ...) {
  UseMethod("as_data_backend")
}
