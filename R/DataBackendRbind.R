#' @include DataBackend.R
DataBackendRbind = R6Class("DataBackendRbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    rows = NULL,
    initialize = function(b1, b2, rows_b1, rows_b2) {
      assert_backend(b1)
      assert_backend(b2)
      assert_subset(rows_b1, b1$rownames)
      assert_subset(rows_b2, b2$rownames)
      pk = b1$primary_key

      formats = intersect(b1$formats, b2$formats)
      if (length(formats) == 0L)
        stopf("There is no common format for the backends to rbind")

      if (pk != b2$primary_key)
        stopf("All backends to rbind must have the same primary_key '%s'", pk)

      self$rows = list(b1 = rows_b1, b2 = rows_b2)
      super$initialize(list(b1 = b1, b2 = b2), b1$primary_key, "data.table")
    },

    data = function(rows, cols, format = self$formats[1L]) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      assert_choice(format, self$formats)

      query_rows = unique(rows)
      query_cols = union(cols, self$primary_key)
      data = rbind(
        private$.data$b1$data(intersect(query_rows, self$rows$b1), query_cols, format = "data.table"),
        private$.data$b2$data(intersect(query_rows, self$rows$b2), query_cols, format = "data.table")
      )
      data[list(rows), intersect(cols, names(data)), nomatch = 0L, on = self$primary_key, with = FALSE]
    },

    head = function(n = 6L) {
      n = assert_count(n, coerce = TRUE)

      data = private$.data$b1$head(n)
      if (nrow(data) < n)
        data = rbind(data, private$.data$b2$head(n - nrow(data)))
      data
    },

    distinct = function(cols) {
      d1 = private$.data$b1$distinct(cols)
      d2 = private$.data$b2$distinct(cols)
      Map(function(nn) union(d1[[nn]], d2[[nn]]), names(d1))
    },

    missing = function(rows, cols) {
      m1 = private$.data$b1$missing(rows, cols)
      m2 = private$.data$b2$missing(rows, cols)
      m1 + m2[match(names(m1), names(m2))]
    }
  ),

  active = list(
    rownames = function() {
      c(self$rows$b1, self$rows$b2)
    },

    colnames = function() {
      private$.data$b1$colnames
    },

    nrow = function() {
      sum(lengths(self$rows))
    },

    ncol = function() {
      private$.data$b1$ncol
    }
  )
)
