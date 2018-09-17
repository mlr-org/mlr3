#' @title Backend Interface
#'
#' @description
#' All objects of type Backend provide the following interface:
#'
#' @section Usage:
#' ```
#' b$data(rows, cols)
#' b$head(n = 6)
#' b$rownames
#' b$colnames
#' b$nrow
#' b$ncol
#' print(b)
#' ```
#' See [BackendDataTable] for an exemplary implementation of this interface.
#'
#' @section Arguments:
#' * `rows` (`integer()` or `character()`):
#'   Vector of row indices to subset rows using the primary key in the data backend.
#' * `cols` (`character()`):
#'   Vector of column names to select specific columns.
#' * `n` (`integer(1)`): Number of rows to return.
#'
#' @section Details:
#' `$data()` returns a slice of the data as [data.table][data.table::data.table()]:
#'   rows are matched by the `primary_key` column, columns are selected by name.
#'
#' `$head()` (`data.table`) returns a [data.table][data.table::data.table()] of the first `n` data rows.
#'
#' `$rownames` (`character(1)`) returns all rownames of `data` as integer or character vector.
#'
#' `$colnames` (`character(1)`) returns all colnames of `data` as character vector.
#'
#' `$nrow` (`integer(1)`) returns the number of total rows.
#'
#' `$ncol` (`integer(1)`) returns the number of total columns, including primary key column.
#' @name Backend
#' @family Backend
NULL
