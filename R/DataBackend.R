#' @title DataBackend
#'
#' @include mlr_reflections.R
#' @include warn_deprecated.R
#'
#' @description
#' This is the abstract base class for data backends.
#'
#' Data backends provide a layer of abstraction for various data storage systems.
#' It is not recommended to work directly with the DataBackend.
#' Instead, all data access is handled transparently via the [Task].
#'
#' This package currently shups with one implementation for backends:
#'
#' * [DataBackendDataTable] which stores the data as [data.table::data.table()].
#'
#' To connect to out-of-memory database management systems such as SQL servers,
#' see the extension package \CRANpkg{mlr3db}.
#'
#' @details
#' The required set of fields and methods to implement a custom `DataBackend` is
#' listed in the respective sections (see [DataBackendDataTable]).
#'
#' @template seealso_databackend
#'
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
    #' @field primary_key (`character(1)`)\cr
    #' Column name of the primary key column of positive and unique integer row ids.
    primary_key = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note: This object is typically constructed via a derived classes, e.g.
    #' [DataBackendDataTable], or via the S3 method
    #' [as_data_backend()].
    #'
    #' @param data (any)\cr
    #'   The format of the input data depends on the specialization. E.g.,
    #'   [DataBackendDataTable] expects a [data.table::data.table()].
    #'
    #' @param primary_key (`character(1)`)\cr
    #'   Each DataBackend needs a way to address rows, which is done via a
    #'   column of unique integer values, referenced here by `primary_key`. The
    #'   use of this variable may differ between backends.
    initialize = function(data, primary_key) {
      private$.data = data
      self$primary_key = assert_string(primary_key)
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    print = function() {
      nr = self$nrow
      cat_cli(cli_h1("{.cls {class(self)[1L]}} ({.val {nr}}x{.val {self$ncol}})"))
      print(self$head(6L), row.names = FALSE, print.keys = FALSE)
      if (nr > 6L) {
        catf("[...] (%i rows omitted)", nr - 6L)
      }
    }
  ),

  active = list(
    #' @field hash (`character(1)`)\cr
    #' Hash (unique identifier) for this object.
    hash = function(rhs) {
      if (missing(rhs)) {
        if (is.na(private$.hash)) {
          private$.hash = private$.calculate_hash()
        }
        return(private$.hash)
      }
      private$.hash = assert_string(rhs)
    },

    #' @template field_col_hashes
    col_hashes = function() {
      cn = setdiff(self$colnames, self$primary_key)
      set_names(sprintf("%s.%s", self$hash, cn), cn)
    }
  ),

  private = list(
    .data = NULL,
    .hash = NA_character_
  )
)
