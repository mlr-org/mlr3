#' @title DataBackend for data.table
#'
#' @description
#' [DataBackend] for \CRANpkg{data.table} which serves as an efficient in-memory data base.
#'
#' @template param_rows
#' @template param_cols
#' @template param_data_format
#' @template param_primary_key
#' @template param_na_rm
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
    #' @field compact_seq `logical(1)`\cr
    #' If `TRUE`, row ids are a natural sequence from 1 to `nrow(data)` (determined internally).
    #' In this case, row lookup uses faster positional indices instead of equi joins.
    compact_seq = FALSE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that `DataBackendDataTable` does not copy the input data, while `as_data_backend()` calls [data.table::copy()].
    #' `as_data_backend()` also takes care about casting to a `data.table()` and adds a primary key column if necessary.
    #'
    #' @param data ([data.table::data.table()])\cr
    #'   The input [data.table()].
    initialize = function(data, primary_key) {
      assert_data_table(data, col.names = "unique")
      super$initialize(setkeyv(data, primary_key), primary_key, data_formats = "data.table")
      assert_choice(primary_key, names(data))
    },

    #' @description
    #' Returns a slice of the data in the specified format.
    #' Currently, the only supported formats are `"data.table"` and `"Matrix"`.
    #' The rows must be addressed as vector of primary key values, columns must be referred to via column names.
    #' Queries for rows with no matching row id and queries for columns with no matching column name are silently ignored.
    #' Rows are guaranteed to be returned in the same order as `rows`, columns may be returned in an arbitrary order.
    #' Duplicated row ids result in duplicated rows, duplicated column names lead to an exception.
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

    #' @description
    #' Retrieve the first `n` rows.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of rows.
    #'
    #' @return [data.table::data.table()] of the first `n` rows.
    head = function(n = 6L) {
      head(private$.data, n)
    },

    #' @description
    #' Returns a named list of vectors of distinct values for each column
    #' specified. If `na_rm` is `TRUE`, missing values are removed from the
    #' returned vectors of distinct values. Non-existing rows and columns are
    #' silently ignored.
    #'
    #' @return Named `list()` of distinct values.
    distinct = function(rows, cols, na_rm = TRUE) {
      cols = intersect(cols, colnames(private$.data))
      if (is.null(rows)) {
        set_names(lapply(cols, function(x) distinct_values(private$.data[[x]], drop = FALSE, na_rm = na_rm)), cols)
      } else {
        lapply(self$data(rows, cols), distinct_values, drop = TRUE, na_rm = na_rm)
      }
    },

    #' @description
    #' Returns the number of missing values per column in the specified slice
    #' of data. Non-existing rows and columns are silently ignored.
    #'
    #' @return Total of missing values per column (named `numeric()`).
    missings = function(rows, cols) {
      data = self$data(rows, cols)
      map_int(data, function(x) sum(is.na(x)))
    }
  ),

  active = list(
    #' @field rownames (`integer()`)\cr
    #' Returns vector of all distinct row identifiers, i.e. the contents of the primary key column.
    rownames = function() {
      private$.data[[self$primary_key]]
    },

    #' @field colnames (`character()`)\cr
    #' Returns vector of all column names, including the primary key column.
    colnames = function() {
      colnames(private$.data)
    },

    #' @field nrow (`integer(1)`)\cr
    #' Number of rows (observations).
    nrow = function() {
      nrow(private$.data)
    },

    #' @field ncol (`integer(1)`)\cr
    #' Number of columns (variables), including the primary key column.
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

#' @param data ([data.frame()])\cr
#'   The input [data.frame()].
#'   Converted to a [data.table::data.table()] automatically.
#' @template param_primary_key
#' @param keep_rownames (`logical(1)` | `character(1)`)\cr
#'   If `TRUE` or a single string, keeps the row names of `data` as a new column.
#'   The column is named like the provided string, defaulting to `"..rownames"` for `keep_rownames == TRUE`.
#'   Note that the created column will be used as a regular feature by the task unless you manually change the column role.
#'   Also see [data.table::as.data.table()].
#' @rdname as_data_backend
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
