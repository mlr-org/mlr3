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

      query_rows = unique(rows)
      query_cols = union(cols, self$primary_key)
      data = rbind(private$.b1$data(query_rows, query_cols), private$.b2$data(query_rows, query_cols))
      data[.(rows), intersect(cols, names(data)), nomatch = 0L, on = self$primary_key, with = FALSE]
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
  assert_set_equal(names(data), backend$colnames)

  ii = wf(data[[backend$primary_key]] %in% backend$rownames)
  if (length(ii))
    stopf("Cannot rbind data to backend: duplicated primary key '%s'", data[[backend$primary_key]][ii])

  tab = merge(col_types(backend$head(1L)), col_types(data), by = "id")[get("type.x") != get("type.y")]
  if (nrow(tab))
    stopf("Cannot rbind to backend: Column types do not match for columns: %s", stri_peek(tab$id))

  BackendRbind$new(backend, BackendDataTable$new(data, primary_key = backend$primary_key))
}
