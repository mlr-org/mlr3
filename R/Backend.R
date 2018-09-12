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

BackendCompound = R6Class("Backend",
  cloneable = FALSE,
  public = list(
    initialize = function(backends) {
      private$.backends = assert_list(backends, types = "Backend")
    },

    data = function(rows, cols) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")

      rbindlist(lapply(private$.backends, function(b) b$data(rows = rows, cols = cols)), fill = TRUE)
    }

    # head = function(n = 6L) {
    #   head(private$.data, n)
    # }
  ),

  active = list(
    rownames = function() {
      unique(unlist(lapply(private$.backends, function(b) b$rownames)))
    },

    colnames = function() {
      unique(unlist(lapply(private$.backends, function(b) b$colnames)))
    },

    nrow = function() {
      length(self$rownames)
    },

    ncol = function() {
      length(self$colnames)
    }
  ),

  private = list(
    .backends = NULL
  )
)

if (FALSE) {
  data = iris
  data$id = 1:150
  b1 = BackendDataTable$new(data[1:100, ], primary_key = "id")
  b2 = BackendDataTable$new(data[101:150, ], primary_key = "id")
  b = BackendCompound$new(list(b1, b2))

  b$data(1:102, "Species")
  b$rownames
  b$colnames
  b$nrow
  b$ncol


  x = data.table(id = c(1:5, 5L), val = letters[1:6], key = "id")
  x[list(3:5), mult = "last"]
}

assert_backend = function(b) {
  assert_class(b, "Backend")
}
