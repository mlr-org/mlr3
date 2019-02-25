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
#' ```
#'
#' * `data` :: [Matrix::Matrix()].
#'
#' * `primary_key` :: `character(1)`\cr
#'   Not supported by this backend. Rows are addresses by their [rownames()].
#'   If the matrix does not have row names, integer row indices are used.
#'
#' Alternatively, use [as_data_backend] on a [Matrix::Matrix()].
#'
#' @inheritSection DataBackend Fields
#' @inheritSection DataBackend Methods
#'
#' @family DataBackend
#' @export
#' @examples
#' requireNamespace("Matrix")
#' data = Matrix::Matrix(sample(0:1, 20, replace = TRUE), ncol = 2)
#' colnames(data) = c("x1", "x2")
#' rownames(data) = paste0("row_", 1:10)
#'
#' b = as_data_backend(data)
#' print(b$head(n = 3, format = "data.table"))
#' print(b$head(n = 3, format = "sparse"))
DataBackendMatrix = R6Class("DataBackendMatrix", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(data, primary_key = NULL) {
      require_namespaces("Matrix")
      assert_class(data, "Matrix")
      assert_unique(colnames(data))

      if (!is.null(rownames(data)))
        assert_unique(rownames(data))

      if (any(dim(data) == 0L))
        stopf("No data in Matrix")

      if (!is.null(primary_key))
        stopf("Primary key column not supported by DataBackendMatrix")

      super$initialize(data, "..row_id", c("data.table", "sparse"))
    },

    data = function(rows, cols, format = self$formats[1L]) {
      assert_choice(format, self$formats)
      assert_atomic_vector(rows)
      assert_unique(cols)
      assert_choice(format, self$formats)

      query_rows = DataBackendMatrix_query_rows(private$.data, rows)
      query_cols = set_intersect(cols, colnames(private$.data))
      data = private$.data[query_rows, query_cols, drop = FALSE]

      switch(format,
        "data.table" = {
          data = as.data.table(as.matrix(data))
          if (self$primary_key %fin% cols)
            data = insert_named(data, set_names(list(query_rows), self$primary_key))
          data
        },
        "sparse" = {
          attr(data, "..row_id") = query_rows
          data
        },
        stopf("Cannot convert to format '%s'", format)
      )
    },

    head = function(n = 6L, format = "data.table") {
      self$data(head(self$rownames, n), self$colnames, format = format)
    },

    distinct = function(cols) {
      query_cols = set_intersect(cols, colnames(private$.data))
      res = set_names(lapply(query_cols, function(col) distinct(private$.data[, col])), query_cols)
      if (self$primary_key %fin% cols) {
        res[[self$primary_key]] = self$rownames
        # res = res[match(names(res), cols, nomatch = 0L)]
      }
      res
    },

    missing = function(rows, cols) {
      query_rows = DataBackendMatrix_query_rows(private$.data, rows)
      query_cols = set_intersect(cols, colnames(private$.data))
      res = apply(private$.data[query_rows, query_cols], 2L, function(x) sum(is.na(x)))
      if (self$primary_key %fin% cols)
        res[self$primary_key] = 0L
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
    .calculate_hash = function() {
      hash(private$.data)
    }
  )
)

DataBackendMatrix_query_rows = function(M, rows) {
  if (is.null(rownames(M)))
    return(filter_oob_index(rows, 1L, nrow(M)))

  set_intersect(assert_character(rows), rownames(M))
}

#' @export
as_data_backend.Matrix = function(data, ...) {
  DataBackendMatrix$new(data)
}
