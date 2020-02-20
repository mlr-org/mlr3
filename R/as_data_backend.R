#' @title Create a Data Backend
#'
#' @description
#' Wraps a [DataBackend] around data.
#'
#' @param data `any`\cr
#'   Data to create a [DataBackend] from.
#'   For a `data.frame()` (this includes `tibble()` from \CRANpkg{tibble} and [data.table::data.table()])
#'   this function creates a [DataBackendDataTable].
#'   See `methods("as_data_backend")` for possible input formats.
#'
#'   Package \CRANpkg{mlr3db} extends this function with a method for lazy table objects implemented in \CRANpkg{dbplyr}.
#'   This allows to interface many different (SQL complicant) data base systems.
#' @param ... (`any`)\cr
#'   Additional arguments passed to the respective [DataBackend] method.
#'
#' @return [DataBackend].
#' @family DataBackend
#' @export
#' @examples
#' # create a new backend using the iris data:
#' as_data_backend(iris)
as_data_backend = function(data, ...) {
  UseMethod("as_data_backend")
}
