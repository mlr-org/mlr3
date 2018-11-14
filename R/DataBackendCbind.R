#' @include DataBackend.R
DataBackendCbind = R6Class("DataBackendCbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      if (b1$primary_key != b2$primary_key)
        stopf("All backends to rbind must have the same primary_key")
      super$initialize(list(b1 = b1, b2 = b2), b1$primary_key, format = intersect(b1$formats, b2$formats))
    },

    data = function(rows, cols) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")

      tab = private$.data$b1$data(rows, cols)

      if (ncol(tab) < length(cols))
        tab = rcbind(tab, remove(private$.data$b2$data(rows, cols), self$primary_key))
      return(tab)
    },

    head = function(n = 6L) {
      x = private$.data$b1$head(n)
      rcbind(x, private$.data$b2$data(rows = x[[self$primary_key]], cols = setdiff(private$.data$b2$colnames, self$primary_key)))
    },

    distinct = function(cols) {
      c(private$.data$b1$distinct(cols), private$.data$b2$distinct(cols))
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
