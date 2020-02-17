#' @title DataBackend for Matrix
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [DataBackend].
#' @include DataBackend.R
#'
#' @description
#' [DataBackend] for \CRANpkg{Matrix}.
#' Data is split into a (numerical) sparse part and a dense part.
#' These parts are automatically merged to a sparse format during `$data()`.
#' Note that merging the data parts may cause data loss during the conversion.
#'
#' @section Construction:
#' ```
#' DataBackendMatrix$new(data, primary_key = NULL)
#' as_data_backend(data, primary_key = NULL, ...)
#' ```
#'
#' * `data` :: [Matrix::Matrix()]\cr
#'   Sparse (numeric) data stored as [Matrix::Matrix()].
#'
#' * `dense` :: [data.frame()].
#'   Dense data, converted to [data.table::data.table()].
#'
#' * `primary_key` :: `character(1)`\cr
#'   Name of the primary key column in `dense`
#'
#' @section Fields:
#' See [DataBackend].
#'
#' @section Methods:
#' See [DataBackend].
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

    head = function(n = 6L) {
      self$data(head(self$rownames, n), self$colnames)
    },

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
    rownames = function(rhs) {
      assert_ro_binding(rhs)
      private$.data$dense[[self$primary_key]]
    },

    colnames = function(rhs) {
      assert_ro_binding(rhs)
      c(colnames(private$.data$dense), colnames(private$.data$sparse))
    },

    nrow = function(rhs) {
      assert_ro_binding(rhs)
      nrow(private$.data$dense)
    },

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
