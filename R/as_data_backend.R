#' @title Create a Data Backend
#'
#' @description
#' Wraps a [DataBackend] around data.
#' \CRANpkg{mlr3} ships with methods for `data.frame` (converted to a [DataBackendDataTable]
#' and `Matrix` from package \CRANpkg{Matrix} (converted to a [DataBackendMatrix]).
#'
#' Additional methods are implemented in the package \CRANpkg{mlr3db}, e.g. to connect
#' to real DBMS like PostgreSQL (via \CRANpkg{dbplyr}) or DuckDB (via \CRANpkg{DBI}/\CRANpkg{duckdb}).
#'
#' @param data `any`\cr
#'   Data to create a [DataBackend] from.
#'   For a `data.frame()` (this includes `tibble()` from \CRANpkg{tibble} and [data.table::data.table()]),
#'   a [DataBackendDataTable] is created.
#'   For objects of type `Matrix` (from package \CRANpkg{Matrix}), a [DataBackendMatrix] is returned.
#'   See `methods("as_data_backend")` for all possible input formats.
#'
#' @template param_primary_key
#'
#' @param ... (`any`)\cr
#'   Additional arguments passed to the respective [DataBackend] method.
#'
#' @return [DataBackend].
#' @family DataBackend
#' @export
#' @examples
#' # create a new backend using the iris data:
#' as_data_backend(iris)
as_data_backend = function(data, primary_key = NULL, ...) {
  UseMethod("as_data_backend")
}

#' @export
as_data_backend.DataBackend = function(data, primary_key = NULL, ...) { # nolint
  data
}
