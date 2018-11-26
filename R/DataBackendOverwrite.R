DataBackendOverwrite = R6Class("DataBackendOverwrite", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      if (b1$primary_key != b2$primary_key)
        stopf("All backends must have the same primary_key")
      super$initialize(list(b1 = b1, b2 = b2), b1$primary_key, intersect(b1$formats, b2$formats))
    },

    data = function(rows, cols, format = self$formats[1L]) {
      assert_choice(format, self$formats)
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      cols = intersect(cols, self$colnames)
      query_cols = union(cols, self$primary_key)

      x = private$.data$b1$data(rows, query_cols)
      y = private$.data$b2$data(rows, query_cols)

      if (ncol(y) > 1L && nrow(y) > 0L)
        x = ujoin(x, y, self$primary_key)

      x[, cols, with = FALSE]
    },

    head = function(n = 6L) {
      x = private$.data$b1$head(n)
      y = private$.data$b2$data(rows = x[[self$primary_key]], cols = names(x))
      ujoin(x, y, self$primary_key)[]
    },

    distinct = function(cols) {
      lapply(self$data(self$rownames, cols), distinct)
    },

    missing = function(rows, cols) {
      unlist(lapply(self$data(rows, cols), function(x) sum(is.na(x))))
    }
  ),

  active = list(
    rownames = function() {
      private$.data$b1$rownames
    },

    colnames = function() {
      private$.data$b1$colnames
    },

    nrow = function() {
      private$.data$b1$nrow
    },

    ncol = function() {
      private$.data$b1$ncol
    }
  )
)
