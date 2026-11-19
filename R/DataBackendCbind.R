#' @include DataBackend.R
DataBackendCbind = R6Class(
  "DataBackendCbind",
  inherit = DataBackend,
  cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      pk = b1$primary_key

      if (pk != b2$primary_key) {
        error_input("All backends to cbind must have the primary_key '%s'", pk)
      }

      super$initialize(list(b1 = b1, b2 = b2), pk)
    },

    data = function(rows, cols) {
      pk = self$primary_key
      qrows = unique(assert_numeric(rows))
      qcols = union(assert_names(cols, type = "unique"), pk)

      data = private$.data$b2$data(qrows, qcols)
      if (ncol(data) < length(qcols)) {
        qcols = c(setdiff(cols, names(data)), pk)
        tmp = private$.data$b1$data(qrows, qcols)
        data = merge(data, tmp, by = pk, all = TRUE, sort = TRUE)
      }

      # duplicate rows / reorder columns
      ijoin(data, rows, intersect(cols, names(data)), pk)
    },

    head = function(n = 6L) {
      rows = head(self$rownames, n)
      self$data(rows = rows, cols = self$colnames)
    },

    distinct = function(rows, cols, na_rm = TRUE) {
      d2 = private$.data$b2$distinct(rows, cols, na_rm = na_rm)
      d1 = private$.data$b1$distinct(rows, setdiff(cols, names(d2)), na_rm = na_rm)

      # rows missing in one of the backends are filled with NA in $data()
      if (!na_rm) {
        pk = self$primary_key
        n = private$.n_rows(rows)
        if (n[["b1"]]) d2 = imap(d2, function(x, nn) if (nn == pk) x else union(x, NA))
        if (n[["b2"]]) d1 = imap(d1, function(x, nn) if (nn == pk) x else union(x, NA))
      }

      res = c(d1, d2)
      res[reorder_vector(names(res), cols)]
    },

    missings = function(rows, cols) {
      m2 = private$.data$b2$missings(rows, cols)
      m1 = private$.data$b1$missings(rows, setdiff(cols, names(m2)))
      if (length(m2) == 0L && length(m1) == 0L) {
        return(set_names(integer()))
      }

      # rows missing in one of the backends are filled with NA in $data()
      pk = self$primary_key
      n = private$.n_rows(rows)
      ii = names(m2) != pk
      m2[ii] = m2[ii] + n[["b1"]]
      ii = names(m1) != pk
      m1[ii] = m1[ii] + n[["b2"]]

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
    },

    # number of queried rows which are only present in one of the backends
    .n_rows = function(rows) {
      pk = self$primary_key
      b1 = private$.data$b1
      b2 = private$.data$b2

      if (is.null(rows)) {
        r1 = b1$rownames
        r2 = b2$rownames
      } else {
        r1 = b1$data(rows, pk)[[pk]]
        r2 = b2$data(rows, pk)[[pk]]
      }

      list(b1 = sum(r1 %nin% r2), b2 = sum(r2 %nin% r1))
    }
  )
)
