#' @title DataBackend for Matrix
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [DataBackend].
#' @include DataBackend.R
#'
#' @description
#' [DataBackend] for \CRANpkg{Matrix}. Data is stored as (sparse) matrix.
#'
#' @section Construction:
#' ```
#' DataBackendMatrix$new(data, primary_key = NULL)
#' as_data_backend(data, primary_key = NULL, ...)
#' ```
#'
#' * `data` :: [Matrix::Matrix()].
#'
#' * `primary_key` :: `character(1)`\cr
#'   Not supported by this backend. Rows are addresses by their row index.
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
#' requireNamespace("Matrix")
#' data = Matrix::Matrix(sample(0:1, 20, replace = TRUE), ncol = 2)
#' colnames(data) = c("x1", "x2")
#'
#' b = as_data_backend(data)
#' b$head()
#' b$data(1:3, b$colnames, data_format = "Matrix")
DataBackendMatrix = R6Class("DataBackendMatrix", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(data, primary_key = NULL) {
      require_namespaces("Matrix")
      assert_class(data, "Matrix")
      assert_names(colnames(data), type = "unique")
      if (!is.null(rownames(data))) {
        rownames(data) = NULL
      }
      if (any(dim(data) == 0L)) {
        stopf("No data in Matrix")
      }
      if (!is.null(primary_key)) {
        stopf("Primary key column not supported by DataBackendMatrix")
      }

      super$initialize(data, "..row_id", c("data.table", "Matrix"))
    },

    data = function(rows, cols, data_format = "data.table") {
      assert_choice(data_format, self$data_formats)
      assert_numeric(rows)
      assert_names(cols, type = "unique")

      query_rows = private$.translate_rows(rows)
      query_cols = intersect(cols, colnames(private$.data))
      data = private$.data[query_rows, query_cols, drop = FALSE]

      switch(data_format,
        "Matrix" = {
          attr(data, "..row_id") = query_rows
          data
        },
        "data.table" = {
          data = as.data.table(as.matrix(data))
          if (self$primary_key %in% cols) {
            data = insert_named(data, set_names(list(query_rows), self$primary_key))
          }
          data
        })
    },

    head = function(n = 6L) {
      self$data(seq_len(n), self$colnames)
    },

    distinct = function(rows, cols, na_rm = TRUE) {
      query_cols = intersect(cols, colnames(private$.data))
      query_rows = if (is.null(rows)) self$rownames else private$.translate_rows(rows)

      res = lapply(query_cols, function(col) distinct_values(private$.data[query_rows, col], drop = FALSE, na_rm = na_rm))
      names(res) = query_cols

      if (self$primary_key %in% cols) {
        res[[self$primary_key]] = query_rows
        res = res[match(cols, names(res), nomatch = 0L)]
      }
      res
    },

    missings = function(rows, cols) {
      query_rows = private$.translate_rows(rows)
      query_cols = intersect(cols, colnames(private$.data))
      res = apply(private$.data[query_rows, query_cols], 2L, function(x) sum(is.na(x)))
      if (self$primary_key %in% cols) {
        res[self$primary_key] = 0L
        res = res[match(cols, names(res), nomatch = 0L)]
      }
      res
    }
  ),

  active = list(
    rownames = function() {
      seq_row(private$.data)
    },

    colnames = function() {
      c(self$primary_key, colnames(private$.data))
    },

    nrow = function() {
      nrow(private$.data)
    },

    ncol = function() {
      ncol(private$.data) + 1L
    }
  ),

  private = list(
    .calculate_hash = function() {
      hash(private$.data)
    },

    .translate_rows = function(rows) {
      keep_in_bounds(rows, 1L, self$nrow)
    })
)

#' @export
as_data_backend.Matrix = function(data, ...) {
  DataBackendMatrix$new(data)
}
