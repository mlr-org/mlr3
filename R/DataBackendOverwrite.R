DataBackendOverwrite = R6Class("DataBackendOverwrite", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      private$.b1 = assert_backend(b1)
      private$.b2 = assert_backend(b2)

      if (b1$primary_key != b2$primary_key)
        stopf("All backends must have the same primary_key")
      self$primary_key = b1$primary_key
      self$formats = intersect(b1$formats, b2$formats)
    },

    data = function(rows, cols) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      cols = intersect(cols, self$colnames)
      query_cols = union(cols, self$primary_key)

      x = private$.b1$data(rows, query_cols)
      y = private$.b2$data(rows, query_cols)

      if (ncol(y) > 1L && nrow(y) > 0L)
        x = ujoin(x, y, self$primary_key)

      x[, cols, with = FALSE]
    },

    head = function(n = 6L) {
      x = private$.b1$head(n)
      y = private$.b2$data(rows = x[[self$primary_key]], cols = names(x))
      ujoin(x, y, self$primary_key)[]
    },

    distinct = function(cols) {
      lapply(self$data(self$rownames, cols), distinct)
    }
  ),

  active = list(
    rownames = function() {
      private$.b1$rownames
    },

    colnames = function() {
      private$.b1$colnames
    },

    nrow = function() {
      private$.b1$nrow
    },

    ncol = function() {
      private$.b1$ncol
    }
  ),

  private = list(
    .b1 = NULL,
    .b2 = NULL
  )
)
