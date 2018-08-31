#' Backend Interface
#'
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


#' Backend for data.table
#'
#' Abstraction for [data.table::data.table()] as an in-memory data base.
#' Returns an object of class [Backend].
#'
#' @section Usage:
#' ```
#' b = BackendDataTable$new(data, primary_key = NULL)
#' ```
#'
#' @section Arguments:
#' * `data` (`data.frame` or `data.table`).
#' * `primary_key` (`character(1)`): Name of the column in `data` which represents a unique
#'     row identifier (as integer or character). If `NULL`, a new column with integer indices is
#'     automatically created.
#'
#' @section Details:
#' `$new()` creates a new object of class [Backend].
#'
#' @export
#' @family Backend
#' @examples
#' b = BackendDataTable$new(data = iris)
#' b$head()
#' b$data(rows = 100:101, cols = "Species")
#'
#' b$nrow
#' head(b$rownames)
#'
#' b$ncol
#' b$colnames
BackendDataTable = R6Class("Backend",
  cloneable = FALSE,
  public = list(
    primary_key = NULL,

    initialize = function(data, primary_key = NULL) {
      assert_data_frame(data, min.rows = 1L, min.cols = 1L)

      if (is.null(primary_key)) {
        rn = attr(data, "row.names")
        data = as.data.table(data)
        if (is.character(rn))
          rn = make.unique(rn)
        data[["..row_id"]] = rn
        self$primary_key = "..row_id"
      } else {
        assert_string(primary_key)
        assert_names(colnames(data), must.include = primary_key)
        assert_atomic_vector(data[[primary_key]], any.missing = FALSE, unique = TRUE)
        self$primary_key = primary_key
        data = as.data.table(data)
      }
      private$.data = setkeyv(data, private$primary_key)
    },

    data = function(rows, cols) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique", subset.of = names(private$.data))

      data = private$.data[list(rows), cols, with = FALSE, nomatch = 0L, on = self$primary_key]
      return(data)
    },

    head = function(n = 6L) {
      head(private$.data, n)
    }
  ),

  active = list(
    rownames = function() {
      private$.data[[self$primary_key]]
    },

    colnames = function() {
      colnames(private$.data)
    },

    nrow = function() {
      nrow(private$.data)
    },

    ncol = function() {
      ncol(private$.data)
    }
  ),

  private = list(
    .data = NULL
  )
)
