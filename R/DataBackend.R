#' @title DataBackend
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the abstract base class for data backends.
#' See [DataBackendDataTable] or [DataBackendMatrix] for exemplary implementations of this interface.
#'
#' Data Backends provide a layer of abstraction for various data storage systems.
#' The required set of operations to implement is listed in the Methods section.
#'
#' Note that all data access is handled transparently via the [Task].
#' It is not recommended to work directly with the DataBackend.
#'
#'
#' @section Construction:
#' ```
#' DataBackend$new(data, primary_key = NULL)
#' ```
#'
#' * `data` :: `any`\cr
#'   The format of the input data depends on the specialization.
#'   E.g., [DataBackendDataTable] expects a [data.table::data.table()] and [DataBackendMatrix] expects a [Matrix::Matrix()]
#'   constructed with the \CRANpkg{Matrix} package.
#' * `primary_key` :: `character(1)`\cr
#'   Each DataBackend needs a way to address rows, which is typically handled by a `primary_key` column of unique values.
#'   The use of this variable may differ between backends.
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
#' * `rownames` :: `integer()` | `character()`\cr
#'   Returns vector of all distinct row identifiers, i.e. the primary key column.
#'
#' * `hash` :: `character(1)`\cr
#'   Returns a unique hash for this backend. This hash is cached.
#'
#' * `data_formats` :: `character()`\cr
#'   Vector of supported data formats.
#'   A specific format of these supported formats can be picked in the `$data()` method.
#'
#' @section Methods:
#' * `data(rows = NULL, cols = NULL, format = "data.table")`\cr
#'   (`integer()` | `character()`, `character()`) -> `any`\cr
#'   Returns a slice of the data in a specific format.
#'   Currently, the only supported format is "data.table".
#'   The rows must be addressed as vector of primary key values, columns must be referred to via column names.
#'   Non-existing rows and columns are silently ignored.
#'
#' * `distinct(rows, cols)`\cr
#'   (`integer()` | `character()`, `character()`) -> named `list()`\cr
#'   Returns a named list of vectors of distinct values for each column specified.
#'   Non-existing columns are silently ignored.
#'   If `rows` is `NULL`, all possible distinct values will be returned, even if they do not occur.
#'   This affects factor-like variables with empty levels.
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
      self$data_formats = assert_subset(data_formats, mlr_reflections$task_data_formats, empty.ok = FALSE)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf("%s (%ix%i)", format(self), self$nrow, self$ncol)
      catf(str_indent("\nPublic:", str_r6_interface(self)))
      print(self$head(6L))
    }
  ),

  active = list(
    hash = function(rhs) {
      if (missing(rhs)) {
        if (is.na(private$.hash))
          private$.hash = private$.calculate_hash()
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
