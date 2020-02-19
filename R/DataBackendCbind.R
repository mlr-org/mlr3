#' @title DataBackendCbind
#' @export
DataBackendCbind = R6Class("DataBackendCbind", inherit = DataBackend, cloneable = FALSE,
  public = list(

    #' @description Column bind two [DataBackends]
    #' @param b1 Backend 1
    #' @param b2 Backend 2
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

    #' @description
    #'   Returns a slice of the data in the specified format. Currently, the
    #'   only supported formats are `"data.table"` and `"Matrix"`. The rows must
    #'   be addressed as vector of primary key values, columns must be referred
    #'   to via column names. Queries for rows with no matching row id and
    #'   queries for columns with no matching column name are silently ignored.
    #'   Rows are guaranteed to be returned in the same order as `rows`, columns
    #'   may be returned in an arbitrary order. Duplicated row ids result in
    #'   duplicated rows, duplicated column names lead to an exception.
    #' @template rows
    #' @template cols
    #' @template data_format
    data = function(rows, cols, data_format = self$data_formats[1L]) {

      pk = self$primary_key
      qrows = unique(assert_numeric(rows))
      qcols = union(assert_names(cols, type = "unique"), self$primary_key)
      assert_choice(data_format, self$data_formats)

      data = private$.data$b2$data(qrows, qcols, data_format = data_format)
      if (ncol(data) < length(qcols)) {
        qcols = setdiff(cols, names(data))
        tmp = private$.data$b1$data(qrows, union(qcols, pk), data_format = data_format)
        data = merge(data, tmp, by = pk, all = TRUE, sort = TRUE)
      }

      # duplicate rows / reorder columns
      data[list(rows), intersect(cols, names(data)), on = pk, with = FALSE, nomatch = NULL]
    },

    #' @description
    #'  Returns the first up-to `n` rows of the data as
    #'  [data.table::data.table()].
    head = function(n = 6L) {
      rows = head(self$rownames, n)
      self$data(rows = rows, cols = self$colnames)
    },

    #' @template distinct
    #' @template rows
    #' @template cols
    #' @template na_rm
    distinct = function(rows, cols, na_rm = TRUE) {
      d2 = private$.data$b2$distinct(rows, cols, na_rm = na_rm)
      d1 = private$.data$b1$distinct(rows, setdiff(cols, names(d2)), na_rm = na_rm)
      res = c(d1, d2)
      res[match(cols, names(res), nomatch = 0L)]
    },

    #' @template missings
    #' @template rows
    #' @template cols
    missings = function(rows, cols) {
      m2 = private$.data$b2$missings(rows, cols)
      m1 = private$.data$b1$missings(rows, setdiff(cols, names(m2)))
      res = c(m1, m2)
      res[match(cols, names(res), nomatch = 0L)]
    }
  ),

  active = list(
    #' @field rownames `integer()`\cr
    #'   Returns vector of all distinct row identifiers, i.e. the primary key
    #'   column.
    rownames = function() {
      union(private$.data$b1$rownames, private$.data$b2$rownames)
    },

    #' @field colnames `character()`\cr
    #'   Returns vector of all column names, including the primary key column.
    colnames = function() {
      union(private$.data$b1$colnames, private$.data$b2$colnames)
    },

    #' @field nrow `integer(1)`\cr
    #'   Number of rows (observations).
    nrow = function() {
      uniqueN(c(private$.data$b1$rownames, private$.data$b2$rownames))
    },

    #' @field ncol `integer(1)`\cr
    #'   Number of columns (variables), including the primary key column.
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
