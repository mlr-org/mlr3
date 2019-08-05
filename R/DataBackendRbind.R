#' @include DataBackend.R
DataBackendRbind = R6Class("DataBackendRbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      pk = b1$primary_key

      data_formats = intersect(b1$data_formats, b2$data_formats)
      if (length(data_formats) == 0L) {
        stopf("There is no common data format for the backends to rbind")
      }

      if (pk != b2$primary_key) {
        stopf("All backends to rbind must have the primary_key '%s'", pk)
      }

      super$initialize(list(b1 = b1, b2 = b2), b1$primary_key, "data.table")
    },

    data = function(rows, cols, data_format = self$data_formats[1L]) {
      pk = self$primary_key
      qrows1 = unique(assert_atomic_vector(rows))
      qcols = union(assert_names(cols, type = "unique"), pk)
      assert_choice(data_format, self$data_formats)

      d1 = private$.data$b1$data(qrows1, qcols, data_format = data_format)
      if (nrow(d1) < length(qrows1)) {
        qrows2 = setdiff(qrows1, d1[[pk]])
        d2 = private$.data$b2$data(qrows2, qcols, data_format = data_format)
        d1 = rbindlist(list(d1, d2), use.names = TRUE, fill = TRUE)
      }

      # duplicate rows / reorder columns
      d1[list(rows), intersect(cols, names(d1)), nomatch = 0L, on = pk, with = FALSE]
    },

    head = function(n = 6L) {
      h1 = private$.data$b1$head(n)
      h2 = private$.data$b2$head(n - nrow(h1))
      rbindlist(list(h1, h2), use.names = TRUE, fill = TRUE)
    },

    distinct = function(rows, cols) {
      cols = intersect(cols, self$colnames)
      d1 = private$.data$b1$distinct(rows, cols)
      d2 = private$.data$b2$distinct(rows, cols)
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
      hash(data$b1$hash, data$b2$hash)
    }
  )
)
