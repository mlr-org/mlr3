# DOCS FIXME

#' @include DataBackend.R
DataBackendRbind = R6Class("DataBackendRbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      if (b1$primary_key != b2$primary_key)
        stopf("All backends to rbind must have the same primary_key")
      super$initialize(list(b1 = b1, b2 = b2), b1$primary_key, intersect(b1$formats, b2$formats))
    },

    data = function(rows, cols, format = self$formats[1L]) {
      assert_choice(format, self$formats)
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")

      query_rows = unique(rows)
      query_cols = union(cols, self$primary_key)
      data = rbind(private$.data$b1$data(query_rows, query_cols), private$.data$b2$data(query_rows, query_cols))
      data[list(rows), intersect(cols, names(data)), nomatch = 0L, on = self$primary_key, with = FALSE]
    },

    head = function(n = 6L) {
      n = assert_count(n, coerce = TRUE)

      data = private$.data$b1$head(n)
      if (nrow(data) != n)
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
      c(private$.data$b1$rownames, private$.data$b2$rownames)
    },

    colnames = function() {
      private$.data$b1$colnames
    },

    nrow = function() {
      private$.data$b1$nrow + private$.data$b2$nrow
    },

    ncol = function() {
      private$.data$b1$ncol
    }
  )
)
