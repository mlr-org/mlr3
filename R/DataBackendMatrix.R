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
      self$format = "data.table"
      private$.data = data
    },

    data = function(rows, cols, format = self$format) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      assert_choice(format, c("data.table", "sparse"))

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

    head = function(n = 6L, format = self$format) {
      self$data(head(self$rownames, n), self$colnames, format = format)
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
as_data_backend.Matrix = function(x, ...) {
  DataBackendMatrix$new(x)
}


if (FALSE) {
}
