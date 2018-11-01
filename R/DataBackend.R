#' @title DataBackend
#'
#' @description
#' This is the abstract base class for data backends.
#' All objects of type `DataBackend` must provide the following interface:
#'
#' @section Usage:
#' ```
#' b$data(rows, cols)
#' b$head(n = 6)
#' b$distinct(cols)
#' b$rownames
#' b$colnames
#' b$nrow
#' b$ncol
#' print(b)
#' ```
#' See [DataBackendDataTable] for an exemplary implementation of this interface.
#'
#' @section Arguments:
#' * `rows` \[`integer()` or `character()`\]:\cr
#'   Vector of row indices to subset rows using the primary key in the data backend.
#' * `cols` \[`character()`\]:\cr
#'   Vector of column names to select.
#' * `n` \[`integer(1)`\]:\cr
#'   Number of rows to return.
#'
#' @section Details:
#' * `$data()` \[[`data.table()`][data.table::data.table()]\] returns a slice of the data:
#'   rows are filtered using the `primary_key` column, columns are selected by name.
#'
#' * `$head()` \[[`data.table()`][data.table::data.table()]\] returns a [data.table][data.table::data.table()] of the first `n` data rows.
#'
#' * `$distinct()` \[`list()`\] returns a named list of distinct values for specified columns `cols`.
#'
#' * `$rownames` \[`character(1)`\] returns all row names of `data` as integer or character vector.
#'
#' * `$colnames` \[`character(1)`\] returns all column names of `data` as character vector.
#'
#' * `$nrow` \[`integer(1)`\] returns the number of total rows.
#'
#' * `$ncol` \[`integer(1)`\] returns the number of total columns, including primary key column.
#'
#' @name DataBackend
#' @family DataBackend
NULL

#' @export
DataBackend = R6Class("DataBackend", cloneable = FALSE,
  public = list(
    print = function() {
      catf("Backend <%s> (%ix%i)", class(self)[1L], self$nrow, self$ncol)
      print(self$head(6L))
    }
  )
)
