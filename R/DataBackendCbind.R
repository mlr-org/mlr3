#' @include DataBackend.R
DataBackendCbind = R6Class("DataBackendCbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    cols = NULL,
    initialize = function(b1, b2, cols_b1, cols_b2) {
      assert_backend(b1)
      assert_backend(b2)
      assert_subset(cols_b1, b1$colnames)
      assert_subset(cols_b2, b2$colnames)
      pk = b1$primary_key

      if (pk != b2$primary_key)
        stopf("All backends to rbind must have the same primary_key '%s'", pk)

      if (any(cols_b1 %in% setdiff(cols_b2, pk)))
        stopf("Ambiguous column membership")

      self$cols = list(b1 = union(pk, cols_b1), b2 = union(pk, cols_b2))
      super$initialize(list(b1 = b1, b2 = b2), pk, "data.table")
    },

    data = function(rows, cols, format = self$formats[1L]) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      assert_choice(format, self$formats)

      tab = private$.data$b1$data(rows, intersect(cols, self$cols$b1), format = "data.table")

      if (ncol(tab) < length(cols)) {
        query_cols = setdiff(intersect(cols, self$cols$b2), self$primary_key)
        tab = ref_cbind(tab, private$.data$b2$data(rows, query_cols, format = "data.table"))
      }
      tab[, intersect(cols, names(tab)), with = FALSE]
    },

    head = function(n = 6L) {
      x = private$.data$b1$head(n)[, self$cols$b1, with = FALSE]
      y = private$.data$b2$data(rows = x[[self$primary_key]], cols = setdiff(self$cols$b2, self$primary_key), format = "data.table")
      ref_cbind(x, y)
    },

    distinct = function(cols) {
      c(
        private$.data$b1$distinct(intersect(cols, self$cols$b1)),
        private$.data$b2$distinct(setdiff(intersect(cols, self$cols$b2), self$primary_key))
      )
    },

    missing = function(rows, cols) {
      c(
        private$.data$b1$missing(rows, intersect(cols, self$cols$b1)),
        private$.data$b2$missing(rows, setdiff(intersect(cols, self$cols$b2), self$primary_key))
      )
    }
  ),

  active = list(
    rownames = function() {
      private$.data$b1$rownames
    },

    colnames = function() {
      c(self$cols$b1, setdiff(self$cols$b2, self$primary_key))
    },

    nrow = function() {
      private$.data$b1$nrow
    },

    ncol = function() {
      sum(lengths(self$cols)) - 1L
    }
  )
)
