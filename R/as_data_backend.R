#' @title Create a Data Backend.
#'
#' @description
#' Wraps a [DataBackend] around data.
#'
#' @param data Data to wrap the backend around.
#'   For a `data.frame()` (this includes `tibble()` from \CRANpkg{tibble} and `data.table()`),
#'   creates a [DataBackendDataTable].
#'   See `methods("as_data_backend")` for possible input formats.
#' @param ... Additional arguments passed to the respective [DataBackend] method.
#'
#' @return [DataBackend].
#' @family DataBackend
#' @export
as_data_backend = function(data, ...) {
  UseMethod("as_data_backend")
}
