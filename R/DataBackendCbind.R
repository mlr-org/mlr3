#' @include DataBackend.R
DataBackendCbind = R6Class("DataBackend", inherit = DataBackend, cloneable = FALSE,
  public = list(
    primary_key = NULL,
    initialize = function(b1, b2) {
      private$.b1 = assert_backend(b1)
      private$.b2 = assert_backend(b2)
      if (b1$primary_key != b2$primary_key)
        stop("All backends to rbind must have the same primary_key")
      self$primary_key = b1$primary_key
    },

    data = function(rows, cols) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")

      tab = private$.b1$data(rows, cols)

      if (ncol(tab) < length(cols))
        tab = rcbind(tab, remove(private$.b2$data(rows, cols), self$primary_key))
      return(tab)
    },

    head = function(n = 6L) {
      x = private$.b1$head(n)
      rcbind(x, private$.b2$data(rows = x[[self$primary_key]], cols = setdiff(private$.b2$colnames, self$primary_key)))
    },

    distinct = function(cols) {
      c(private$.b1$distinct(cols), private$.b2$distinct(cols))
    }
  ),

  active = list(
    rownames = function() {
      private$.b1$rownames
    },

    colnames = function() {
      c(private$.b1$colnames, setdiff(private$.b2$colnames, self$primary_key))
    },

    nrow = function() {
      private$.b1$nrow
    },

    ncol = function() {
      private$.b1$ncol + private$.b2$ncol - 1L
    }
  ),

  private = list(
    .b1 = NULL,
    .b2 = NULL
  )
)
