BackendCbind = R6Class("Backend",
  cloneable = FALSE,
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

      if (ncol(tab) < length(cols)) {
        extra_cols = remove(private$.b2$data(rows, cols), self$primary_key)
        if (ncol(extra_cols))
          tab = cbind(tab, extra_cols)
      }
      return(tab)
    },

    head = function(n = 6L) {
      x = private$.b1$head(n)
      cbind(x, private$.b2$data(rows = x[[self$primary_key]], cols = setdiff(private$.b2$colnames, self$primary_key)))
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

backend_cbind = function(backend, data) {
  assert_backend(backend)
  assert_data_frame(data)
  assert_set_equal(data[[backend$primary_key]], backend$rownames)
  ii = wf(names(data) %in% setdiff(backend$colnames, backend$primary_key))
  if (length(ii))
    stopf("Cannot cbind data to backend: duplicated colname '%s'", names(data)[ii])
  BackendCbind$new(backend, BackendDataTable$new(data, primary_key = backend$primary_key))
}
