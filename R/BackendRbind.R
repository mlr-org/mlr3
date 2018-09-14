BackendRbind = R6Class("Backend",
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

      rbind(private$.b1$data(rows, cols), private$.b2$data(rows, cols))
    },

    head = function(n = 6L) {
      n = assert_count(n, coerce = TRUE)

      data = private$.b1$head(n)
      if (nrow(data) != n)
        data = rbind(data, private$.b2$head(n - nrow(data)))
      data
    }
  ),

  active = list(
    rownames = function() {
      c(private$.b1$rownames, private$.b2$rownames)
    },

    colnames = function() {
      private$.b1$colnames
    },

    nrow = function() {
      private$.b1$nrow + private$.b2$nrow
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

backend_rbind = function(backend, data) {
  assert_backend(backend)
  assert_data_frame(data)
  assert_set_equal(backend$colnames, names(data))
  BackendRbind$new(backend, BackendDataTable$new(data, primary_key = backend$primary_key))
}
