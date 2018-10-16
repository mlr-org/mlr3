#' @include DataBackend.R
#' @export
#' @keywords internal
DataBackendRbind = R6Class("DataBackend", inherit = DataBackend, cloneable = FALSE,
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
      data[list(rows), intersect(cols, names(data)), nomatch = 0L, on = self$primary_key, with = FALSE]
    },

    head = function(n = 6L) {
      n = assert_count(n, coerce = TRUE)

      data = private$.b1$head(n)
      if (nrow(data) != n)
        data = rbind(data, private$.b2$head(n - nrow(data)))
      data
    },

    distinct = function(cols) {
      d1 = private$.b1$distinct(cols)
      d2 = private$.b2$distinct(cols)
      setNames(lapply(names(d1), function(nn) union(d1[[nn]], d2[[nn]])), names(d1))
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
