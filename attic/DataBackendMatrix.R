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
      private$.data = data
    },

    data = function(rows, cols, format = "data.table") {
      assert_integer(rows)
      assert_names(cols, type = "unique")
      assert_choice(format, c("data.table", "sparse"))

      query_rows = rows[!is.na(rows) & rows >= 1L & rows <= nrow(private$.data)]
      query_cols = intersect(cols, colnames(private$.data))
      data = private$.data[query_rows, query_cols, drop = FALSE]
      if (self$primary_key %in% cols) {
        # NB: this is inefficent, but also an unusual query. Most queries from mlr3 do not include the primary key column, except unit tests
        data = cbind(Matrix::Matrix(query_rows, ncol = 1L, dimnames = list(NULL, self$primary_key), sparse = TRUE), data)
      }
      if (format == "sparse") data else sparse_to_dt(data)
    },

    head = function(n = 6L) {
      n = min(n, nrow(private$.data))
      self$data(seq_len(n), self$colnames, format = "data.table")
    },

    distinct = function(cols) {
      query_cols = intersect(cols, colnames(private$.data))
      res = setNames(lapply(query_cols, function(col) distinct(private$.data[, col])), query_cols)
      if (self$primary_key %in% cols)
        res[[self$primary_key]] = seq_row(private$.data)
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
    .data = NULL
  )
)

sparse_to_dt = function(x) {
  if (ncol(x) == 0L)
    return(data.table())
  as.data.table(as.matrix(x))
}


if (FALSE) {
  data = Matrix::Matrix(0, nrow = 10, ncol = 100, sparse = TRUE)
  colnames(data) = sprintf("cn%04i", seq_len(ncol(data)))

  self = DataBackendMatrix$new(data)
  private = private(self)
  self$nrow
  self$ncol
  self$rownames
  self$colnames

  self$head()
  rows = 1:5
  cols = c("cn0001", "cn0002")
  self$data(rows, cols)
  self$data(rows, cols, format = "data.table")
  self$data(rows, c("..row_id", cols))
  self$data(rows, c("..row_id", cols), format = "data.table")
}
