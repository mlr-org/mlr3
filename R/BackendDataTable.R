#' @title Backend for data.table
#'
#' @description
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
#' @name BackendDataTable
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
NULL

#' @export
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
      assert_names(cols, type = "unique")
      cols = intersect(cols, self$colnames)

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
