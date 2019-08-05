#' @include DataBackend.R
DataBackendCbind = R6Class("DataBackendCbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      pk = b1$primary_key

      data_formats = intersect(b1$data_formats, b2$data_formats)
      if (length(data_formats) == 0L) {
        stopf("There is no common data format for the backends to cbind")
      }

      if (pk != b2$primary_key) {
        stopf("All backends to rbind must have the primary_key '%s'", pk)
      }

      super$initialize(list(b1 = b1, b2 = b2), pk, "data.table")
    },

    data = function(rows, cols, data_format = self$data_formats[1L]) {
      pk = self$primary_key
      qrows = unique(assert_atomic_vector(rows))
      qcols1 = union(assert_names(cols, type = "unique"), self$primary_key)
      assert_choice(data_format, self$data_formats)

      d1 = private$.data$b1$data(qrows, qcols1, data_format = data_format)
      qcols2 = setdiff(qcols1, colnames(d1))
      if (length(qcols2) > 0L) {
        d2 = private$.data$b2$data(qrows, union(qcols2, pk), data_format = data_format)
        d1 = merge(d1, d2, by = pk, all = TRUE, sort = TRUE)
      }

      # duplicate rows / reorder columns
      d1[list(rows), intersect(cols, names(d1)), on = pk, with = FALSE, nomatch = 0L]
    },

    head = function(n = 6L) {
      rows = head(self$rownames, n)
      self$data(rows = rows, cols = self$colnames)
    },

    distinct = function(rows, cols) {
      d1 = private$.data$b1$distinct(rows, cols)
      d2 = private$.data$b2$distinct(rows, setdiff(cols, names(d1)))
      res = c(d1, d2)
      res[match(cols, names(res), nomatch = 0L)]
    },

    missings = function(rows, cols) {
      m1 = private$.data$b1$missings(rows, cols)
      m2 = private$.data$b2$missings(rows, setdiff(cols, names(m1)))
      res = c(m1, m2)
      res[match(cols, names(res), nomatch = 0L)]
    }
  ),

  active = list(
    rownames = function() {
      union(private$.data$b1$rownames, private$.data$b2$rownames)
    },

    colnames = function() {
      union(private$.data$b1$colnames, private$.data$b2$colnames)
    },

    nrow = function() {
      uniqueN(c(private$.data$b1$rownames, private$.data$b2$rownames))
    },

    ncol = function() {
      uniqueN(c(private$.data$b1$colnames, private$.data$b2$colnames))
    }
  ),

  private = list(
    .calculate_hash = function() {
      data = private$.data
      hash(data$b1$hash, data$b2$hash)
    }
  )
)
