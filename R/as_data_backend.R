#' @title Create a Data Backend
#'
#' @description
#' Wraps a [DataBackend] around data.
#'
#' @param data `any`\cr
#'   Data to create a [DataBackend] for.
#'   For a `data.frame()` (this includes `tibble()` from \CRANpkg{tibble} and [data.table::data.table()])
#'   this function creates a [DataBackendDataTable].
#'   See `methods("as_data_backend")` for possible input formats.
#'   Note that third-party packages may extend this functionality.
#' @param ... :: `any`\cr
#'   Additional arguments passed to the respective [DataBackend] method.
#'
#' @return [DataBackend].
#' @family DataBackend
#' @name as_data_backend
#' @export
#' @examples
#' # create a new backend using the iris data:
#' as_data_backend(iris)
as_data_backend = function(data, ...) {
  UseMethod("as_data_backend")
}
