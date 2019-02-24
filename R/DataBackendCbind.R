#' @include DataBackend.R
DataBackendCbind = R6Class("DataBackendCbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    rows = NULL,
    cols = NULL,

    initialize = function(b1, b2, cols_b1, cols_b2) {
      assert_backend(b1)
      assert_backend(b2)
      assert_subset(cols_b1, b1$colnames, fmatch = TRUE)
      assert_subset(cols_b2, b2$colnames, fmatch = TRUE)
      pk = b1$primary_key

      formats = intersect(b1$formats, b2$formats)
      if (length(formats) == 0L)
        stopf("There is no common format for the backends to cbind")

      if (pk != b2$primary_key)
        stopf("All backends to rbind must have the same primary_key '%s'", pk)

      i = which(cols_b1 %fin% setdiff(cols_b2, pk))
      if (length(i))
        stopf("Ambiguous column membership: %s", str_collapse(cols_b1[i], quote = "'"))

      self$rows = set_intersect(b1$rownames, b2$rownames)
      self$cols = list(b1 = set_union(pk, cols_b1), b2 = set_union(pk, cols_b2))
      super$initialize(list(b1 = b1, b2 = b2), pk, "data.table")
    },

    data = function(rows, cols, format = self$formats[1L]) {
      qrows = set_intersect(assert_atomic_vector(rows), self$rows)
      qcols = set_union(assert_names(cols, type = "unique"), self$primary_key)
      assert_choice(format, self$formats)

      d1 = private$.data$b1$data(qrows, set_intersect(qcols, self$cols$b1), format = format)

      if (ncol(d1) < length(qcols)) {
        d2 = private$.data$b2$data(qrows, set_intersect(qcols, self$cols$b2), format = format)
        d1 = d1[d2, on = self$primary_key, nomatch = 0L]
      }
      d1[list(rows), set_intersect(cols, names(d1)), with = FALSE, on = self$primary_key, nomatch = 0L]
    },

    head = function(n = 6L) {
      rows = head(self$rows, n)
      self$data(rows = rows, cols = self$colnames)
    },

    distinct = function(cols) {
      c(
        private$.data$b1$distinct(set_intersect(cols, self$cols$b1)),
        private$.data$b2$distinct(set_diff(set_intersect(cols, self$cols$b2), self$primary_key))
      )
    },

    missing = function(rows, cols) {
      rows = intersect(rows, self$rows)
      c(
        private$.data$b1$missing(rows, set_intersect(cols, self$cols$b1)),
        private$.data$b2$missing(rows, set_diff(set_intersect(cols, self$cols$b2), self$primary_key))
      )
    }
  ),

  active = list(
    rownames = function() {
      self$rows
    },

    colnames = function() {
      c(self$cols$b1, set_diff(self$cols$b2, self$primary_key))
    },

    nrow = function() {
      length(self$rows)
    },

    ncol = function() {
      sum(lengths(self$cols)) - 1L
    }
  ),

  private = list(
    .calculate_hash = function() {
      data = private$.data
      hash(c(data$b1$hash, data$b2$hash))
    }
  )
)
