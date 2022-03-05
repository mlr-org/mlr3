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
#' @param data (any)\cr
#'   Data to create a [DataBackend] from.
#'   For a `data.frame()` (this includes `tibble()` from \CRANpkg{tibble} and [data.table::data.table()]),
#'   a [DataBackendDataTable] is created.
#'   For objects of type `Matrix` (from package \CRANpkg{Matrix}), a [DataBackendMatrix] is returned.
#'   See `methods("as_data_backend")` for all possible input formats.
#'
#' @template param_primary_key
#'
#' @param ... (any)\cr
#'   Additional arguments passed to the respective [DataBackend] method.
#'
#' @return [DataBackend].
#'
#' @template seealso_databackend
#' @export
#' @examples
#' # create a new backend using the penguins data:
#' as_data_backend(palmerpenguins::penguins)
as_data_backend = function(data, primary_key = NULL, ...) {
  UseMethod("as_data_backend")
}


#' @export
as_data_backend.DataBackend = function(data, primary_key = NULL, ...) { # nolint
  data
}


#' @param data ([data.frame()])\cr
#'   The input [data.frame()].
#'   Automatically converted to a [data.table::data.table()].
#'
#' @param keep_rownames (`logical(1)` | `character(1)`)\cr
#'   If `TRUE` or a single string, keeps the row names of `data` as a new column.
#'   The column is named like the provided string, defaulting to `"..rownames"` for `keep_rownames == TRUE`.
#'   Note that the created column will be used as a regular feature by the task unless you manually change the column role.
#'   Also see [data.table::as.data.table()].
#'
#' @rdname as_data_backend
#' @export
as_data_backend.data.frame = function(data, primary_key = NULL, keep_rownames = FALSE, ...) { # nolint
  assert_data_frame(data, min.cols = 1L, col.names = "unique")

  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  data = as.data.table(data, keep.rownames = keep_rownames)
  compact_seq = FALSE

  if (is.character(primary_key)) {
    assert_string(primary_key)
    assert_choice(primary_key, colnames(data))
    assert_integer(data[[primary_key]], any.missing = FALSE, unique = TRUE)
  } else {
    if (is.null(primary_key)) {
      row_ids = seq_row(data)
      compact_seq = TRUE
    } else if (is.integer(primary_key)) {
      row_ids = assert_integer(primary_key, len = nrow(data), any.missing = FALSE, unique = TRUE)
    } else {
      stopf("Argument 'primary_key' must be NULL, a column name or a vector of ids")
    }

    primary_key = "..row_id"
    data = insert_named(data, list("..row_id" = row_ids))
  }

  b = DataBackendDataTable$new(data, primary_key)
  b$compact_seq = compact_seq

  return(b)
}
