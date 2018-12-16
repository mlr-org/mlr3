#' @title DataBackend
#'
#' @description
#' This is the abstract base class for data backends.
#' All objects of type `DataBackend` must provide the following interface:
#'
#' @section Usage:
#' ```
#' b = as_data_backend(data)
#' b$data(rows, cols)
#' b$head(n = 6)
#' b$distinct(cols)
#' b$missing(rows, cols)
#' b$rownames
#' b$colnames
#' b$nrow
#' b$ncol
#' print(b)
#' ```
#' See [DataBackendDataTable] for an exemplary implementation of this interface.
#'
#' @section Arguments:
#' * `data`:\cr
#'   Data to wrap the backend around. Typically a `data.frame`.
#' * `rows` (`integer()` or `character()`):\cr
#'   Vector of row indices to subset rows using the primary key in the data backend.
#' * `cols` (`character()`):\cr
#'   Vector of column names to select.
#' * `n` (`integer(1)`):\cr
#'   Number of rows to return.
#'
#' @section Details:
#' * `as_data_backend()` wraps a `DataBackend` around the provided data. See `methods("as_data_backend")` for
#'   possible input formats.
#'
#' * `$data()` ([data.table::data.table()]) returns a slice of the data:
#'   rows are filtered using the `primary_key` column, columns are selected by name.
#'
#' * `$head()` ([data.table::data.table()]) returns a table of the first `n` data rows.
#'
#' * `$distinct()` (named `list()`) returns distinct values for columns `cols`.
#'
#' * `$missing()` (named `integer()`) returns a named integer with the number of missing values per column.
#'
#' * `$rownames` (`integer()` | `character()`) returns all row names of `data`.
#'
#' * `$colnames` (`character()`) returns all column names of `data`.
#'
#' * `$nrow` (`integer(1)`) returns the number of total rows.
#'
#' * `$ncol` (`integer(1)`) returns the number of total columns, including primary key column.
#'
#' @name DataBackend
#' @aliases as_data_backend
#' @family DataBackend
NULL

#' @export
DataBackend = R6Class("DataBackend", cloneable = FALSE,
  public = list(
    primary_key = NULL,
    formats = character(0L),
    initialize = function(data, primary_key, formats = "data.table") {
      private$.data = data
      self$primary_key = assert_string(primary_key)
      self$formats = assert_subset(formats, mlr_reflections$backend_formats, empty.ok = FALSE)
    },
    print = function() {
      catf("DataBackend <%s> (%ix%i)", class(self)[1L], self$nrow, self$ncol)
      catf(str_indent(initial = "\nPublic: ", str_r6_interface(self)))
      print(self$head(6L))
    }
  ),
  private = list(.data = NULL)
)

#' @export
as_data_backend = function(data, ...) {
  UseMethod("as_data_backend")
}
