#' @title DataBackend
#' @format [R6Class] object
#' @description
#' This is the abstract base class for data backends.
#' See [DataBackendDataTable] for an exemplary implementation of this interface.
#'
#' @section Usage:
#'
#' ```
#' # Construction
#' see ?as_data_backend
#'
#' # Members
#' b$colnames
#' b$ncol
#' b$nrow
#' b$rownames
#'
#' # Methods
#' b$data(rows, cols)
#' b$distinct(cols)
#' b$head(n = 6)
#' b$missing(rows, cols)
#' print(b)
#' ```
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
#' * `$colnames` (`character()`) returns all column names of `data`.
#' * `$data()` ([data.table::data.table]) returns a slice of the data: rows
#'   are filtered using the `primary_key` column, columns are selected by name.
#' * `$distinct()` (named `list()`) returns distinct values for columns `cols`.
#' * `$head()` ([data.table::data.table]) returns a table of the first `n` data rows.
#' * `$missing()` (named `integer()`) returns a named integer with the number of
#'   missing values per column.
#' * `$ncol` (`integer(1)`) returns the number of total columns, including primary key column.
#' * `$nrow` (`integer(1)`) returns the number of total rows.
#' * `$rownames` (`integer()` | `character()`) returns all row names of `data`.
#'
#' @family DataBackend
#' @aliases as_data_backend
#' @name DataBackend
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/DataBackend.html)
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
      catf("<%s> (%ix%i)", class(self)[1L], self$nrow, self$ncol)
      catf(str_indent("\nPublic:", str_r6_interface(self)))
      print(self$head(6L))
    }
  ),
  private = list(.data = NULL)
)
