#' @title DataBackend for Matrix
#'
#' @description
#' Abstraction for [`Matrix()`][Matrix::Matrix()]. Data is stored as (sparse) matrix.
#' Supports two output formats:
#'
#' * `"data.table"` (default): Returns a `data.table::data.table()`. The primary key is returned as a regular column.
#' * `"sparse"` (native): Returns a `Matrix::Matrix()`. The primary key is either stored as row names, or as additional attribute (`"row_i"`).
#'
#' @section Usage:
#' ```
#' # Construction
#' b = DataBackendMatrix$new(data)
#' ```
#' The interface is described in [DataBackend].
#'
#' @section Arguments:
#' * `data` \[[`Matrix`][Matrix::Matrix()]\].\cr
#'   A (sparse) matrix. If `data` has row names, these will be used as primary key.
#'   Integer keys (`1:nrow(data)`) are used otherwise.
#'
#' @name DataBackendMatrix
#' @family DataBackend
#' @examples
#' requireNamespace("Matrix")
#' data = Matrix::Matrix(sample(0:1, 20, replace = TRUE), ncol = 2)
#' colnames(data) = c("x1", "x2")
#' rownames(data) = paste0("row_", 1:10)
#'
#' b = as_data_backend(data)
#' print(b$head(n = 3, format = "data.table"))
#' print(b$head(n = 3, format = "sparse"))
NULL

#' @include DataBackend.R
#' @export
DataBackendMatrix = R6Class("DataBackendMatrix", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(data) {
      require_namespaces("Matrix")
      assert_class(data, "Matrix")
      assert_names(colnames(data), type = "unique")
      if (!is.null(rownames(data)))
        assert_names(rownames(data), type = "unique")

      self$primary_key = "..row_id"
      self$formats = c("data.table", "sparse")
      private$.data = data
    },

    data = function(rows, cols, format = "data.table") {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      assert_choice(format, self$formats)

      rn = rownames(private$.data)

      if (is.null(rn)) {
        query_rows = filter_oob_index(rows, 1L, nrow(private$.data))
      } else {
        assert_character(rows)
        query_rows = intersect(rows, rn)
      }

      query_cols = intersect(cols, colnames(private$.data))
      data = private$.data[query_rows, query_cols, drop = FALSE]

      switch(format,
        "data.table" = {
          data = as.data.table(as.matrix(data))
          if (self$primary_key %in% cols)
            data = insert(data, setNames(list(query_rows), self$primary_key))
          data
        },
        "sparse" = {
          if (is.integer(query_rows))
            attr(data, "row_i") = query_rows
          data
        },
        stopf("Cannot convert to format '%s'", format)
      )
    },

    head = function(n = 6L, format = "data.table") {
      self$data(head(self$rownames, n), self$colnames, format = format)
    },

    distinct = function(cols) {
      query_cols = intersect(cols, colnames(private$.data))
      res = setNames(lapply(query_cols, function(col) distinct(private$.data[, col])), query_cols)
      if (self$primary_key %in% cols)
        res[[self$primary_key]] = self$rownames
      res
    }
  ),

  active = list(
    rownames = function() {
      rownames(private$.data) %??% seq_row(private$.data)
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
    .data = NULL
  )
)


#' @export
as_data_backend.Matrix = function(data, ...) {
  DataBackendMatrix$new(data)
}
