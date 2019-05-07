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
#' as_data_backend(data, primary_key = NULL, ...)
#' ```
#'
#' * `data` :: [data.table::data.table()]\cr
#'   The input [data.table::data.table()].
#'
#' * `primary_key` :: `character(1)`\cr
#'   Name of the primary key column.
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
#'
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
DataBackendDataTable = R6Class("DataBackendDataTable", inherit = DataBackend,
  cloneable = FALSE,
  public = list(
    compact_seq = FALSE,

    initialize = function(data, primary_key) {
      assert_data_table(data)
      super$initialize(setkeyv(data, primary_key), primary_key, data_formats = "data.table")
      assert_choice(primary_key, names(data))
    },

    data = function(rows, cols, data_format = "data.table") {
      assert_choice(data_format, self$data_formats)
      assert_atomic_vector(rows)
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

    distinct = function(rows, cols) {
      cols = intersect(cols, colnames(private$.data))
      if (is.null(rows)) {
        set_names(lapply(cols, function(x) distinct(private$.data[[x]], drop = FALSE)), cols)
      } else {
        lapply(self$data(rows, cols), distinct)
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
      hash(list(self$compact_seq, private$.data))
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
