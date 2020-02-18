#' @title DataBackend for data.table
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [DataBackend].
#' @include DataBackend.R
#'
#' @description
#' [DataBackend] for \CRANpkg{data.table} as an in-memory data base.
#'
#' @section Construction:
#' ```
#' DataBackendDataTable$new(data, primary_key = NULL)
#' as_data_backend(data, primary_key = NULL, keep_rownames = FALSE, ...)
#' ```
#'
#' * `data` :: [data.frame()]\cr
#'   The input [data.frame()].
#'   Converted to a [data.table::data.table()] automatically.
#'
#' * `primary_key` :: `character(1)`\cr
#'   Name of the primary key column.
#'
#' * `keep_rownames` :: `logical(1)` | `character(1)`\cr
#'   If `TRUE` or a single string, keeps the row names of `data` as a new column.
#'   The column is named like the provided string, defaulting to `"..rownames"` for `keep_rownames == TRUE`.
#'   Note that the created column will be used as a regular feature by the task unless you manually change the column role.
#'   See also [data.table::as.data.table()].
#'
#' `DataBackendDataTable` does not copy the input data, while `as_data_backend` calls [data.table::copy()].
#' `as_data_backend` creates a primary key column as integer column if `primary_key` is `NULL.`
#'
#' @section Fields:
#' See [DataBackend].
#'
#' @section Methods:
#' See [DataBackend].
#'
#' @family DataBackend
#' @export
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
#'
#' # alternative construction
#' as_data_backend(iris)
DataBackendDataTable = R6Class("DataBackendDataTable", inherit = DataBackend,
  cloneable = FALSE,
  public = list(
    compact_seq = FALSE,

    initialize = function(data, primary_key) {
      assert_data_table(data, col.names = "unique")
      super$initialize(setkeyv(data, primary_key), primary_key, data_formats = "data.table")
      assert_choice(primary_key, names(data))
    },

    data = function(rows, cols, data_format = "data.table") {
      assert_choice(data_format, self$data_formats)
      assert_numeric(rows)
      assert_names(cols, type = "unique")
      cols = intersect(cols, colnames(private$.data))

      if (self$compact_seq) {
        # https://github.com/Rdatatable/data.table/issues/3109
        rows = keep_in_bounds(rows, 1L, nrow(private$.data))
        data = private$.data[rows, cols, with = FALSE]
      } else {
        data = private$.data[list(rows), cols, with = FALSE, nomatch = NULL, on = self$primary_key]
      }
      return(data)
    },

    head = function(n = 6L) {
      head(private$.data, n)
    },

    distinct = function(rows, cols, na_rm = TRUE) {
      cols = intersect(cols, colnames(private$.data))
      if (is.null(rows)) {
        set_names(lapply(cols, function(x) distinct_values(private$.data[[x]], drop = FALSE, na_rm = na_rm)), cols)
      } else {
        lapply(self$data(rows, cols), distinct_values, drop = TRUE, na_rm = na_rm)
      }
    },

    missings = function(rows, cols) {
      data = self$data(rows, cols)
      map_int(data, function(x) sum(is.na(x)))
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
    .calculate_hash = function() {
      hash(self$compact_seq, private$.data)
    }
  )
)

#' @export
as_data_backend.data.frame = function(data, primary_key = NULL, keep_rownames = FALSE, ...) {
  assert_data_frame(data, min.cols = 1L, col.names = "unique")

  if (!isFALSE(keep_rownames)) {
    if (isTRUE(keep_rownames)) {
      keep_rownames = "..rownames"
    } else {
      assert_string(keep_rownames)
    }
  }

  data = as.data.table(data, keep.rownames = keep_rownames)

  if (!is.null(primary_key)) {
    assert_string(primary_key)
    assert_choice(primary_key, colnames(data))
    assert_integer(data[[primary_key]], any.missing = FALSE, unique = TRUE)
    b = DataBackendDataTable$new(data, primary_key)
  } else {
    data = insert_named(as.data.table(data), list("..row_id" = seq_row(data)))
    b = DataBackendDataTable$new(data, primary_key = "..row_id")
    b$compact_seq = TRUE
  }

  return(b)
}
