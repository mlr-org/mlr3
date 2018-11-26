#' @title DataBackend for data.table
#'
#' @description
#' Abstraction for [`data.table()`][data.table::data.table()] as an in-memory data base.
#' Returns an object of class [DataBackend].
#'
#' @section Usage:
#' ```
#' # Construction
#' b = DataBackendDataTable$new(data, primary_key)
#' b = as_data_backend(data, primary_key = NULL)
#' ```
#' The interface is described in [DataBackend].
#'
#' @section Arguments:
#' * `data` \[[`data.frame()`][base::data.frame()]\].
#'
#' * `primary_key` \[`character(1)`\]:\cr
#'   Name of the column in `data` which represents a unique
#'   row identifier (as integer or character).
#'   If `NULL, the constructor [as_data_backend()] automatically creates an integer column of primary keys.
#'
#' @name DataBackendDataTable
#' @family DataBackend
#' @examples
#' data = as.data.table(iris)
#' data$id = seq_len(nrow(iris))
#' b = DataBackendDataTable$new(data = data, primary_key = "id")
#' print(b)
#' b$head()
#' b$data(rows = 100:101, cols = "Species")
#'
#' b$nrow
#' head(b$rownames)
#'
#' b$ncol
#' b$colnames
NULL

#' @include DataBackend.R
#' @export
DataBackendDataTable = R6Class("DataBackendDataTable", inherit = DataBackend,
  cloneable = FALSE,
  public = list(
    compact_seq = FALSE,

    initialize = function(data, primary_key) {
      assert_data_table(data)
      super$initialize(setkeyv(data, primary_key), primary_key)
      assert_choice(primary_key, names(data))
    },

    data = function(rows, cols, format = self$formats[1L]) {
      assert_choice(format, self$formats)
      assert_names(cols, type = "unique")
      cols = intersect(cols, colnames(private$.data))

      if (self$compact_seq) {
        # https://github.com/Rdatatable/data.table/issues/3109
        rows = filter_oob_index(rows, 1L, nrow(private$.data))
        data = private$.data[rows, cols, with = FALSE]
      } else {
        assert_atomic_vector(rows)
        data = private$.data[list(rows), cols, with = FALSE, nomatch = 0L, on = self$primary_key]
      }
      return(data)
    },

    head = function(n = 6L) {
      head(private$.data, n)
    },

    distinct = function(cols) {
      cols = intersect(cols, colnames(private$.data))
      lapply(private$.data[, cols, with = FALSE], distinct)
    },

    missing = function(rows, cols) {
      assert_names(cols, type = "unique")
      cols = intersect(cols, colnames(private$.data))
      if (length(cols) == 0L)
        return(setNames(integer(), character()))

      if (self$compact_seq) {
        rows = filter_oob_index(rows, 1L, nrow(private$.data))
        data = private$.data[rows, lapply(.SD, function(x) sum(is.na(x))), .SDcols = cols]
      } else {
        assert_atomic_vector(rows)
        data = private$.data[list(rows), lapply(.SD, function(x) sum(is.na(x))), on = self$primary_key, nomatch = 0L, .SDcols = cols]
      }
      if (nrow(data) == 0L)
        return(setNames(integer(ncol(data)), names(data)))

      unlist(data, recursive = FALSE)
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
  )
)

#' @export
as_data_backend.data.frame = function(data, primary_key = NULL, ...) {
  assert_data_frame(data, min.rows = 1L, min.cols = 1L)

  if (!is.null(primary_key)) {
    assert_atomic_vector(data[[primary_key]], any.missing = FALSE, unique = TRUE)
    assert_string(primary_key)
    assert_names(colnames(data), must.include = primary_key)
    return(DataBackendDataTable$new(as.data.table(data), primary_key))
  }

  rn = attr(data, "row.names")
  if (is.character(rn)) {
    data = insert_named(as.data.table(data), list("..row_id" = make.unique(rn)))
    return(DataBackendDataTable$new(data, primary_key = "..row_id"))
  }

  data = insert_named(as.data.table(data), list("..row_id" = seq_row(data)))
  b = DataBackendDataTable$new(data, primary_key = "..row_id")
  b$compact_seq = TRUE
  b
}
