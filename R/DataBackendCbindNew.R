#' @include DataBackend.R
DataBackendCbindNew = R6Class("DataBackendCbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    cols = NULL,
    initialize = function(b1, b2, cols_b1, cols_b2) {
      assert_backend(b1)
      assert_backend(b2)
      assert_subset(cols_b1, b1$colnames)
      assert_subset(cols_b2, b2$colnames)

      if (any(cols_b1 %in% cols_b2))
        stopf("Ambiguous column membership")

      if (b1$primary_key != b2$primary_key)
        stopf("All backends to rbind must have the same primary_key")
      formats = intersect(b1$formats, b2$formats)
      if (length(formats) == 0L)
        stopf("There is no common format for the backends to cbind")

      self$cols = list(b1 = union(b1$primary_key, cols_b1), b2 = union(b1$primary_key, cols_b2))
      super$initialize(list(b1 = b1, b2 = b2), b1$primary_key, formats = formats)
    },

    data = function(rows, cols, format = self$formats[1L]) {
      assert_choice(format, self$formats)
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")

      tab = private$.data$b1$data(rows, intersect(cols, self$cols$b1))

      if (ncol(tab) < length(cols))
        tab = ref_cbind(tab, private$.data$b2$data(rows, setdiff(intersect(cols, self$cols$b2), self$primary_key)))
      tab[, intersect(cols, names(tab)), with = FALSE]
    },

    head = function(n = 6L) {
      x = private$.data$b1$head(n)[, self$cols$b1, with = FALSE]
      ref_cbind(x, private$.data$b2$data(rows = x[[self$primary_key]], cols = setdiff(self$cols$b2, self$primary_key)))
    },

    distinct = function(cols) {
      c(
        private$.data$b1$distinct(intersect(cols, self$cols$b1)),
        private$.data$b2$distinct(setdiff(intersect(cols, self$cols$b2), self$primary_key))
      )
    },

    missing = function(rows, cols) {
      m1 = private$.data$b1$missing(rows, intersect(cols, self$cols$b1))
      m2 = private$.data$b2$missing(rows, setdiff(intersect(cols, self$cols$b2), self$primary_key))
      c(m1, m2)
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
      length(self$cols$b1) + length(self$cols$b2) - 1L
    }
  )
)
