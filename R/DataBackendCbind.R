#' @include DataBackend.R
DataBackendCbind = R6Class("DataBackendCbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      if (b1$primary_key != b2$primary_key)
        stopf("All backends to rbind must have the same primary_key")
      formats = intersect(b1$formats, b2$formats)
      if (length(formats) == 0L)
        stopf("There is no common format for the backends to cbind")
      super$initialize(list(b1 = b1, b2 = b2), b1$primary_key, formats = formats)
    },

    data = function(rows, cols, format = self$formats[1L]) {
      assert_choice(format, self$formats)
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")

      tab = private$.data$b1$data(rows, cols)

      if (ncol(tab) < length(cols))
        ref_cbind(tab, remove_named(private$.data$b2$data(rows, cols), self$primary_key))
      return(tab)
    },

    head = function(n = 6L) {
      x = private$.data$b1$head(n)
      ref_cbind(x, private$.data$b2$data(rows = x[[self$primary_key]], cols = setdiff(private$.data$b2$colnames, self$primary_key)))
    },

    distinct = function(cols) {
      c(private$.data$b1$distinct(cols), private$.data$b2$distinct(cols))
    },

    missing = function(rows, cols) {
      c(private$.data$b1$missing(rows, cols), private$.data$b2$missing(rows, cols))
    }
  ),

  active = list(
    rownames = function() {
      private$.data$b1$rownames
    },

    colnames = function() {
      c(private$.data$b1$colnames, setdiff(private$.data$b2$colnames, self$primary_key))
    },

    nrow = function() {
      private$.data$b1$nrow
    },

    ncol = function() {
      private$.data$b1$ncol + private$.data$b2$ncol - 1L
    }
  )
)
