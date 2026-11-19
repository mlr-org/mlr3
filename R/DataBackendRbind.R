#' @include DataBackend.R
DataBackendRbind = R6Class(
  "DataBackendRbind",
  inherit = DataBackend,
  cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      pk = b1$primary_key

      if (pk != b2$primary_key) {
        error_input("All backends to rbind must have the primary_key '%s'", pk)
      }

      super$initialize(list(b1 = b1, b2 = b2), pk)
    },

    data = function(rows, cols) {
      pk = self$primary_key
      qrows = unique(assert_numeric(rows))
      qcols = union(assert_names(cols, type = "unique"), pk)

      data = private$.data$b2$data(qrows, qcols)
      if (nrow(data) < length(qrows)) {
        qrows = setdiff(rows, data[[pk]])
        tmp = private$.data$b1$data(qrows, qcols)
        data = rbindlist(list(data, tmp), use.names = TRUE, fill = TRUE)
      }

      # duplicate rows / reorder columns
      ijoin(data, rows, intersect(cols, names(data)), pk)
    },

    head = function(n = 6L) {
      rows = head(self$rownames, n)
      self$data(rows, cols = self$colnames)
    },

    distinct = function(rows, cols, na_rm = TRUE) {
      b1 = private$.data$b1
      b2 = private$.data$b2
      cols = intersect(cols, self$colnames)
      d1 = b1$distinct(rows, cols, na_rm = na_rm)
      d2 = b2$distinct(rows, cols, na_rm = na_rm)
      res = set_names(map(cols, function(nn) union(d1[[nn]], d2[[nn]])), cols)

      # rows of a backend without the column are filled with NA in $data()
      if (!na_rm) {
        n = private$.n_rows(rows)
        filled = c(if (n[["b2"]]) setdiff(cols, b2$colnames), if (n[["b1"]]) setdiff(cols, b1$colnames))
        res[filled] = map(res[filled], function(x) union(x, NA))
      }

      res
    },

    missings = function(rows, cols) {
      b1 = private$.data$b1
      b2 = private$.data$b2
      cols = intersect(cols, self$colnames)
      m1 = as.list(b1$missings(rows, cols))
      m2 = as.list(b2$missings(rows, cols))

      # rows of a backend without the column are filled with NA in $data()
      n = private$.n_rows(rows)
      m1[setdiff(cols, b1$colnames)] = n[["b1"]]
      m2[setdiff(cols, b2$colnames)] = n[["b2"]]

      set_names(map_int(cols, function(nn) m1[[nn]] + m2[[nn]]), cols)
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
    },

    # number of queried rows served by each backend, rows in both backends are served by b2
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

      list(b1 = sum(r1 %nin% r2), b2 = length(r2))
    }
  )
)
