#' @title DataBackend
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' This is the abstract base class for data backends.
#'
#' Data Backends provide a layer of abstraction for various data storage systems.
#' It is not recommended to work directly with the DataBackend.
#' Instead, all data access is handled transparently via the [Task].
#'
#' To connect to out-of-memory database management systems such as SQL servers, see \CRANpkg{mlr3db}.
#'
#' The required set of fields and methods to implement a custom DataBackend is listed in the respective sections.
#' See [DataBackendDataTable] or [DataBackendMatrix] for exemplary implementations of the interface.
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived classes, e.g. [DataBackendDataTable] or [DataBackendMatrix],
#' or via the S3 method [as_data_backend()].
#'
#' ```
#' DataBackend$new(data, primary_key = NULL, data_formats = "data.table")
#' ```
#'
#' * `data` :: `any`\cr
#'   The format of the input data depends on the specialization.
#'   E.g., [DataBackendDataTable] expects a [data.table::data.table()] and [DataBackendMatrix] expects a [Matrix::Matrix()]
#'   constructed with the \CRANpkg{Matrix} package.
#'
#' * `primary_key` :: `character(1)`\cr
#'   Each DataBackend needs a way to address rows, which is done via a column of unique values, referenced here by `primary_key`.
#'   The use of this variable may differ between backends.
#'
#' * data_formats (`character()`)\cr
#'   Set of supported formats, e.g. `"data.table"` or `"Matrix"`.
#'
#' @section Fields:
#' * `nrow` :: `integer(1)`\cr
#'   Number of rows (observations).
#'
#' * `ncol` :: `integer(1)`\cr
#'   Number of columns (variables), including the primary key column.
#'
#' * `colnames` :: `character()`\cr
#'   Returns vector of all column names, including the primary key column.
#'
#' * `rownames` :: (`integer()` | `character()`)\cr
#'   Returns vector of all distinct row identifiers, i.e. the primary key column.
#'
#' * `hash` :: `character(1)`\cr
#'   Returns a unique hash for this backend. This hash is cached.
#'
#' * `data_formats` :: `character()`\cr
#'   Vector of supported data formats.
#'   A specific format can be chosen in the `$data()` method.
#'
#' @section Methods:
#' * `data(rows = NULL, cols = NULL, format = "data.table")`\cr
#'   (`integer()` | `character()`, `character()`) -> `any`\cr
#'   Returns a slice of the data in the specified format.
#'   Currently, the only supported formats are `"data.table"` and `"Matrix"`.
#'   The rows must be addressed as vector of primary key values, columns must be referred to via column names.
#'   Queries for rows with no matching row id and queries for columns with no matching column name are silently ignored.
#'   Rows are guaranteed to be returned in the same order as `rows`, columns may be returned in an arbitrary order.
#'   Duplicated row ids result in duplicated rows, duplicated column names lead to an exception.
#'
#' * `distinct(rows, cols, na_rm = TRUE)`\cr
#'   (`integer()` | `character()`, `character()`, `logical(1)`) -> named `list()`\cr
#'   Returns a named list of vectors of distinct values for each column specified.
#'   If `na_rm` is `TRUE`, missing values are removed from the returned vectors of distinct values.
#'   Non-existing rows and columns are silently ignored.
#'
#'   If `rows` is `NULL`, all possible distinct values will be returned, even if the value is not present in the data.
#'   This affects factor-like variables with empty levels, if supported by the backend.
#'
#' * `head(n = 6)`\cr
#'   `integer(1)` -> [data.table::data.table()]\cr
#'   Returns the first up-to `n` rows of the data as [data.table::data.table()].
#'
#' * `missings(rows, cols)`\cr
#'   (`integer()` | `character()`, `character()`) -> named `integer()`\cr
#'   Returns the number of missing values per column in the specified slice of data.
#'   Non-existing rows and columns are silently ignored.
#'
#' @family DataBackend
#' @seealso
#' Extension Packages: \CRANpkg{mlr3db}
#' @export
#' @examples
#' data = data.table::data.table(id = 1:5, x = runif(5), y = sample(letters[1:3], 5, replace = TRUE))
#'
#' b = DataBackendDataTable$new(data, primary_key = "id")
#' print(b)
#' b$head(2)
#' b$data(rows = 1:2, cols = "x")
#' b$distinct(rows = b$rownames, "y")
#' b$missings(rows = b$rownames, cols = names(data))
DataBackend = R6Class("DataBackend", cloneable = FALSE,
  public = list(
    primary_key = NULL,
    data_formats = NULL,

    initialize = function(data, primary_key, data_formats = "data.table") {
      private$.data = data
      self$primary_key = assert_string(primary_key)
      self$data_formats = assert_subset(data_formats, mlr_reflections$data_formats, empty.ok = FALSE)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      nr = self$nrow
      catf("%s (%ix%i)", format(self), nr, self$ncol)
      print(self$head(6L), row.names = FALSE)
      if (nr > 6L) {
        catf("[...] (%i rows omitted)", nr - 6L)
      }
    }
  ),

  active = list(
    hash = function(rhs) {
      if (missing(rhs)) {
        if (is.na(private$.hash)) {
          private$.hash = private$.calculate_hash()
        }
        return(private$.hash)
      }
      private$.hash = assert_string(rhs)
    }
  ),

  private = list(
    .data = NULL,
    .hash = NA_character_
  )
)
