#' @include DataBackend.R
DataBackendCbind = R6Class("DataBackendCbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      pk = b1$primary_key

      data_formats = intersect(b1$data_formats, b2$data_formats)
      if ("data.table" %nin% data_formats) {
        stopf("There is supported data format for the backends to cbind (supported: 'data.table')")
      }

      if (pk != b2$primary_key) {
        stopf("All backends to rbind must have the primary_key '%s'", pk)
      }

      super$initialize(list(b1 = b1, b2 = b2), pk, "data.table")
    },

    data = function(rows, cols, data_format = "data.table") {
      pk = self$primary_key
      qrows = unique(assert_numeric(rows))
      qcols = union(assert_names(cols, type = "unique"), pk)
      assert_choice(data_format, self$data_formats)

      data = private$.data$b2$data(qrows, qcols, data_format = data_format)
      tmp = if (ncol(data) < length(qcols)) {
        qcols = c(setdiff(cols, names(data)), pk)
        private$.data$b1$data(qrows, qcols, data_format = data_format)
      }

      virtual_cbind(data, tmp, rows, cols, pk)
    },

    head = function(n = 6L) {
      rows = head(self$rownames, n)
      self$data(rows = rows, cols = self$colnames)
    },

    distinct = function(rows, cols, na_rm = TRUE) {
      d2 = private$.data$b2$distinct(rows, cols, na_rm = na_rm)
      d1 = private$.data$b1$distinct(rows, setdiff(cols, names(d2)), na_rm = na_rm)
      res = c(d1, d2)
      res[reorder_vector(names(res), cols)]
    },

    missings = function(rows, cols) {
      m2 = private$.data$b2$missings(rows, cols)
      m1 = private$.data$b1$missings(rows, setdiff(cols, names(m2)))
      if (length(m2) == 0L && length(m1) == 0L) {
        return(set_names(integer()))
      }

      res = c(m1, m2)
      res[reorder_vector(names(res), cols)]
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
    },

    col_hashes = function() {
      insert_named(private$.data$b1$col_hashes, private$.data$b2$col_hashes)
    }
  ),

  private = list(
    .calculate_hash = function() {
      data = private$.data
      calculate_hash(data$b1$hash, data$b2$hash)
    }
  )
)

#' Cbind two data formats
#' d2 can be NULL
#' must return the rows in the correct order, columns can be in any order
#' @export
virtual_cbind = function(d1, d2, rows, cols, primary_key, ...) { # nolint
  UseMethod("virtual_cbind")
}

#' @export
virtual_cbind.data.table = function(d1, d2, rows, cols, primary_key) { # nolint
  if (!is.null(d2)) {
    data = merge(d1, d2, by = primary_key, all = TRUE, sort = TRUE)
  } else {
    data = d1
  }
    # duplicate rows / reorder columns
  ijoin(data, rows,  intersect(cols, names(data)), primary_key)
}

