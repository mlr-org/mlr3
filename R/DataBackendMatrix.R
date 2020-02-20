#' @title DataBackend for Matrix
#'
#' @description
#' [DataBackend] for \CRANpkg{Matrix}.
#' Data is split into a (numerical) sparse part and an optional dense part.
#' These parts are automatically merged to a sparse format during `$data()`.
#' Note that merging both parts potentially comes with a data loss, as all
#' dense columns are converted to numeric columns.
#'
#' @template param_rows
#' @template param_cols
#' @template param_data_format
#' @template param_primary_key
#' @template param_na_rm
#'
#' @family DataBackend
#' @export
#' @examples
#' requireNamespace("Matrix")
#' data = Matrix::Matrix(sample(0:1, 20, replace = TRUE), ncol = 2)
#' colnames(data) = c("x1", "x2")
#' dense = data.frame(..row_id = 1:10, num = runif(10), fact = sample(c("a", "b"), 10, replace = TRUE))
#'
#' b = as_data_backend(data, dense, "..row_id")
#' b$head()
#' b$data(1:3, b$colnames, data_format = "Matrix")
#' b$data(1:3, b$colnames, data_format = "data.table")
DataBackendMatrix = R6Class("DataBackendMatrix", inherit = DataBackend, cloneable = FALSE,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param data [Matrix::Matrix()]\cr
    #'   The input [Matrix::Matrix()].
    #' @param dense [data.frame()].
    #'   Dense data, converted to [data.table::data.table()].
    initialize = function(data, dense = NULL, primary_key = NULL) {
      require_namespaces("Matrix")
      assert_class(data, "Matrix")
      assert_names(colnames(data), type = "unique")
      rownames(data) = NULL

      assert_data_frame(dense, nrows = nrow(data))
      assert_names(names(dense), type = "unique")
      assert_choice(primary_key, names(dense))

      assert_disjunct(colnames(data), colnames(dense))

      super$initialize(data = list(sparse = data, dense = as.data.table(dense)), primary_key, data_formats = c("Matrix", "data.table"))
    },

    #' @description
    #' Returns a slice of the data in the specified format.
    #' Currently, the only supported formats are `"data.table"` and `"Matrix"`.
    #' The rows must be addressed as vector of primary key values, columns must be referred to via column names.
    #' Queries for rows with no matching row id and queries for columns with no matching column name are silently ignored.
    #' Rows are guaranteed to be returned in the same order as `rows`, columns may be returned in an arbitrary order.
    #' Duplicated row ids result in duplicated rows, duplicated column names lead to an exception.
    data = function(rows, cols, data_format = "data.table") {
      assert_numeric(rows)
      assert_names(cols, type = "unique")
      assert_choice(data_format, self$data_formats)

      rows = private$.translate_rows(rows)
      cols_sparse = intersect(cols, colnames(private$.data$sparse))
      cols_dense = intersect(cols, colnames(private$.data$dense))

      sparse = private$.data$sparse[rows, cols_sparse, drop = FALSE]
      dense = private$.data$dense[rows, cols_dense, with = FALSE]

      if (data_format == "data.table") {
        data = cbind(as.data.table(as.matrix(sparse)), dense)
        setcolorder(data, intersect(cols, names(data)))
      } else {
        qassertr(dense, c("n", "f"))

        factors = names(which(map_lgl(dense, is.factor)))
        if (length(factors)) {
          # create list of dummy matrices
          dummies = imap(dense[, factors, with = FALSE], function(x, nn) {
            contrasts = contr.treatment(levels(x), sparse = TRUE)
            X = contrasts[match(x, rownames(contrasts)),, drop = FALSE]
            colnames(X) = sprintf("%s_%s", nn, colnames(contrasts))
            X
          })

          # update the column vector with new dummy names (this preserves the order)
          cols = Reduce(function(cols, name) replace_with(cols, name, colnames(dummies[[name]])),
            names(dummies), init = cols)
          dense = remove_named(dense, factors)
        } else {
          dummies = NULL
        }

        dense = if (nrow(dense)) as.matrix(dense) else NULL
        data = do.call(cbind, c(list(sparse, dense), dummies))
        data[, match(cols, colnames(data), nomatch = 0L), drop = FALSE]
      }

      data
    },

    #' @description
    #' Retrieve the first `n` rows.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of rows.
    head = function(n = 6L) {
      self$data(head(self$rownames, n), self$colnames)
    },

    #' @description
    #' Returns a named list of vectors of distinct values for each column
    #' specified. If `na_rm` is `TRUE`, missing values are removed from the
    #' returned vectors of distinct values. Non-existing rows and columns are
    #' silently ignored.
    distinct = function(rows, cols, na_rm = TRUE) {
      rows = if (is.null(rows)) self$rownames else private$.translate_rows(rows)
      cols_sparse = intersect(cols, colnames(private$.data$sparse))
      cols_dense = intersect(cols, colnames(private$.data$dense))

      res = c(
        set_names(lapply(cols_sparse, function(col) distinct_values(private$.data$sparse[rows, col], na_rm = na_rm)), cols_sparse),
        lapply(private$.data$dense[rows, cols_dense, with = FALSE], distinct_values, na_rm = na_rm)
      )

      res[match(cols, names(res), nomatch = 0L)]
    },

    #' @description
    #' Returns the number of missing values per column in the specified slice
    #' of data. Non-existing rows and columns are silently ignored.
    missings = function(rows, cols) {
      rows = private$.translate_rows(rows)
      cols_sparse = intersect(cols, colnames(private$.data$sparse))
      cols_dense = intersect(cols, colnames(private$.data$dense))

      res = c(
        apply(private$.data$sparse[rows, cols_sparse, drop = FALSE], 2L, function(x) sum(is.na(x))),
        private$.data$dense[, map_int(.SD, function(x) sum(is.na(x))), .SDcols = cols_dense]
      )

      res[match(cols, names(res), nomatch = 0L)]
    }
  ),

  active = list(
    #' @field rownames (`integer()`)\cr
    #' Returns vector of all distinct row identifiers, i.e. the contents of the primary key column.
    rownames = function(rhs) {
      assert_ro_binding(rhs)
      private$.data$dense[[self$primary_key]]
    },

    #' @field colnames (`character()`)\cr
    #' Returns vector of all column names, including the primary key column.
    colnames = function(rhs) {
      assert_ro_binding(rhs)
      c(colnames(private$.data$dense), colnames(private$.data$sparse))
    },

    #' @field nrow (`integer(1)`)\cr
    #' Number of rows (observations).
    nrow = function(rhs) {
      assert_ro_binding(rhs)
      nrow(private$.data$dense)
    },

    #' @field ncol (`integer(1)`)\cr
    #' Number of columns (variables), including the primary key column.
    ncol = function(rhs) {
      assert_ro_binding(rhs)
      ncol(private$.data$sparse) + ncol(private$.data$dense)
    }
  ),

  private = list(
    .calculate_hash = function() {
      hash(private$.data)
    },

    .translate_rows = function(rows) {
      private$.data$dense[list(rows), nomatch = NULL, on = self$primary_key, which = TRUE]
    }
  )
)

#' @param data ([Matrix::Matrix()])\cr
#'   The input [Matrix::Matrix()].
#' @param dense ([data.frame()]).
#'   Dense data, converted to [data.table::data.table()].
#' @template param_primary_key
#' @rdname as_data_backend
#' @export
as_data_backend.Matrix = function(data, dense = NULL, primary_key = NULL, ...) {
  require_namespaces("Matrix")
  assert_data_frame(dense, null.ok = TRUE)
  assert_string(primary_key, null.ok = TRUE)

  if (is.null(dense)) {
    if (!is.null(primary_key)) {
      stopf("Primary key '%s' must be provided in 'dense'", primary_key)
    }
    primary_key = "..row_id"
    dense = setnames(data.table(seq_row(data)), primary_key)
  } else {
    if (is.null(primary_key)) {
      stopf("Primary key '%s' must be specified as column name of 'dense'", primary_key)
    }
    assert_choice(primary_key, colnames(dense))
  }

  DataBackendMatrix$new(data, dense, primary_key)
}
