#' @include DataBackend.R
DataBackendRbind = R6Class("DataBackendRbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      pk = b1$primary_key

      if (pk != b2$primary_key) {
        stopf("All backends to rbind must have the primary_key '%s'", pk)
      }

      super$initialize(list(b1 = b1, b2 = b2), pk)
    },

    data = function(rows, cols, data_format) {
      pk = self$primary_key
      qrows = unique(assert_numeric(rows))
      qcols = union(assert_names(cols, type = "unique"), pk)
      if (!missing(data_format)) warn_deprecated("DataBackendRbind$data argument 'data_format'")

      data = private$.data$b2$data(qrows, qcols)
      if (nrow(data) < length(qrows)) {
        qrows = setdiff(rows, data[[pk]])
        tmp = private$.data$b1$data(qrows, qcols)
        data = rbindlist(list(data, tmp), use.names = TRUE, fill = TRUE)
      }

      # duplicate rows / reorder columns
      ijoin(data, rows,  intersect(cols, names(data)), pk)
    },

    head = function(n = 6L) {
      rows = head(self$rownames, n)
      self$data(rows, cols = self$colnames)
    },

    distinct = function(rows, cols, na_rm = TRUE) {
      cols = intersect(cols, self$colnames)
      d1 = private$.data$b1$distinct(rows, cols, na_rm = na_rm)
      d2 = private$.data$b2$distinct(rows, cols, na_rm = na_rm)
      set_names(map(cols, function(nn) union(d1[[nn]], d2[[nn]])), cols)
    },

    missings = function(rows, cols) {
      cols = intersect(cols, self$colnames)
      m1 = as.list(private$.data$b1$missings(rows, cols))
      m2 = as.list(private$.data$b2$missings(rows, cols))
      set_names(map_int(cols, function(nn) m1[[nn]] %??% 0L + m2[[nn]] %??% 0L), cols)
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
      calculate_hash(data$b1$hash, data$b2$hash)
    }
  )
)
