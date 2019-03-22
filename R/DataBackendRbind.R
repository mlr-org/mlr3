#' @include DataBackend.R
DataBackendRbind = R6Class("DataBackendRbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    rows = NULL,
    cols = NULL,

    initialize = function(b1, b2, rows_b1, rows_b2) {
      assert_backend(b1)
      assert_backend(b2)
      assert_subset(rows_b1, b1$rownames)
      assert_subset(rows_b2, b2$rownames)
      pk = b1$primary_key

      data_formats = intersect(b1$data_formats, b2$data_formats)
      if (length(data_formats) == 0L)
        stopf("There is no common data format for the backends to rbind")

      if (pk != b2$primary_key)
        stopf("All backends to rbind must have the same primary_key '%s'", pk)

      i = which(rows_b1 %in% rows_b2)
      if (length(i))
        stopf("Ambiguous row ids: %s", str_collapse(rows_b1[i], quote = "'", n = 10L))

      self$rows = list(b1 = rows_b1, b2 = rows_b2)
      self$cols = intersect(b1$colnames, b2$colnames)
      super$initialize(list(b1 = b1, b2 = b2), b1$primary_key, "data.table")
    },

    data = function(rows, cols, data_format = self$data_formats[1L]) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      assert_choice(data_format, self$data_formats)
      cols = intersect(cols, self$cols)

      query_rows = unique(rows)
      query_cols = union(cols, self$primary_key)
      data = rbind(
        private$.data$b1$data(intersect(query_rows, self$rows$b1), query_cols, data_format = data_format),
        private$.data$b2$data(intersect(query_rows, self$rows$b2), query_cols, data_format = data_format)
      )
      data[list(rows), intersect(cols, names(data)), nomatch = 0L, on = self$primary_key, with = FALSE]
    },

    head = function(n = 6L) {
      n = assert_count(n, coerce = TRUE)

      cols = self$cols
      h1 = private$.data$b1$head(n)
      h2 = private$.data$b2$head(n)

      if (nrow(h1) < n)
        rbind(h1[, cols, with = FALSE], head(h2[, cols, with = FALSE], n - nrow(h1)))
      else
        h1[, cols, with = FALSE]
    },

    distinct = function(rows, cols) {
      cols = intersect(cols, self$cols)
      d1 = private$.data$b1$distinct(rows, cols)
      d2 = private$.data$b2$distinct(rows, cols)
      Map(function(nn) union(d1[[nn]], d2[[nn]]), names(d1))
    },

    missings = function(rows, cols) {
      cols = intersect(cols, self$cols)
      m1 = private$.data$b1$missings(rows, cols)
      m2 = private$.data$b2$missings(rows, cols)
      m1 + m2[match(names(m1), names(m2))]
    }
  ),

  active = list(
    rownames = function() {
      c(self$rows$b1, self$rows$b2)
    },

    colnames = function() {
      self$cols
    },

    nrow = function() {
      sum(lengths(self$rows))
    },

    ncol = function() {
      length(self$cols)
    }
  ),

  private = list(
    .calculate_hash = function() {
      data = private$.data
      hash(c(data$b1$hash, data$b2$hash))
    }
  )
)
