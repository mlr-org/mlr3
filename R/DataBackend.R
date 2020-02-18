#' @title DataBackend
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the abstract base class for data backends.
#'
#' Data backends provide a layer of abstraction for various data storage systems.
#' It is not recommended to work directly with the DataBackend.
#' Instead, all data access is handled transparently via the [Task].
#'
#' This package comes with two implementations for backends:
#' * [DataBackendDataTable] which stores the data as [data.table::data.table()].
#' * [DataBackendMatrix] which stores the data as sparse [Matrix::sparseMatrix()].
#' To connect to out-of-memory database management systems such as SQL servers,
#' see the extension package \CRANpkg{mlr3db}.
#'
#' The required set of fields and methods to implement a custom `DataBackend` is
#' listed in the respective sections (see [DataBackendDataTable] or
#' [DataBackendMatrix] for exemplary implementations of the interface).
#'
#' @family DataBackend
#' @seealso
#' Extension Packages: \CRANpkg{mlr3db}
#' @export
#' @examples
#' data = data.table::data.table(id = 1:5, x = runif(5),
#'   y = sample(letters[1:3], 5, replace = TRUE))
#'
#' b = DataBackendDataTable$new(data, primary_key = "id")
#' print(b)
#' b$head(2)
#' b$data(rows = 1:2, cols = "x")
#' b$distinct(rows = b$rownames, "y")
#' b$missings(rows = b$rownames, cols = names(data))
DataBackend = R6Class("DataBackend", cloneable = FALSE,
  public = list(
    #' @field primary_key
    #'   Each DataBackend needs a way to address rows, which is done via a
    #'   column of unique integer values, referenced here by `primary_key`. The
    #'   use of this variable may differ between backends.
    primary_key = NULL,

    #' @field data_formats
    #'   Set of supported formats, e.g. `"data.table"` or `"Matrix"`.
    data_formats = NULL,

    #' @description Create a DataBackend object
    #'
    #' Note: This object is typically constructed via a derived classes, e.g.
    #' [DataBackendDataTable] or [DataBackendMatrix], or via the S3 method
    #' [as_data_backend()].
    #'
    #' @param data `any`\cr
    #'   The format of the input data depends on the specialization. E.g.,
    #'   [DataBackendDataTable] expects a [data.table::data.table()] and
    #'   [DataBackendMatrix] expects a [Matrix::Matrix()] constructed with the
    #'   \CRANpkg{Matrix} package.
    #' @param primary_key `character(1)`\cr
    #'   Each DataBackend needs a way to address rows, which is done via a
    #'   column of unique integer values, referenced here by `primary_key`. The
    #'   use of this variable may differ between backends.
    #' @param data_formats `character()`\cr
    #'   Set of supported formats, e.g. `"data.table"` or `"Matrix"`.
    initialize = function(data, primary_key, data_formats = "data.table") {
      private$.data = data
      self$primary_key = assert_string(primary_key)
      self$data_formats = assert_subset(data_formats, mlr_reflections$data_formats, empty.ok = FALSE)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer for [DataBackend]
    print = function() {
      nr = self$nrow
      catf("%s (%ix%i)", format(self), nr, self$ncol)
      print(self$head(6L), row.names = FALSE, print.keys = FALSE)
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
