#' @title DataBackend for data.table
#'
#' @description
#' [DataBackend] for \CRANpkg{data.table} which serves as an efficient in-memory data base.
#'
#' @template param_rows
#' @template param_cols
#' @template param_data_format
#' @template param_primary_key
#' @template param_na_rm
#'
#' @template seealso_databackend
#' @export
#' @examples
#' data = as.data.table(palmerpenguins::penguins)
#' data$id = seq_len(nrow(palmerpenguins::penguins))
#' b = DataBackendDataTable$new(data = data, primary_key = "id")
#' print(b)
#' b$head()
#' b$data(rows = 100:101, cols = "species")
#'
#' b$nrow
#' head(b$rownames)
#'
#' b$ncol
#' b$colnames
#'
#' # alternative construction
#' as_data_backend(palmerpenguins::penguins)
DataBackendDataTable = R6Class("DataBackendDataTable", inherit = DataBackend,
  cloneable = FALSE,
  public = list(
    #' @field compact_seq `logical(1)`\cr
    #' If `TRUE`, row ids are a natural sequence from 1 to `nrow(data)` (determined internally).
    #' In this case, row lookup uses faster positional indices instead of equi joins.
    compact_seq = FALSE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that `DataBackendDataTable` does not copy the input data, while `as_data_backend()` calls [data.table::copy()].
    #' `as_data_backend()` also takes care about casting to a `data.table()` and adds a primary key column if necessary.
    #'
    #' @param data ([data.table::data.table()])\cr
    #'   The input [data.table()].
    initialize = function(data, primary_key) {
      assert_data_table(data, col.names = "unique")
      super$initialize(setkeyv(data, primary_key), primary_key, data_formats = "data.table")
      ii = match(primary_key, names(data))
      if (is.na(ii)) {
        stopf("Primary key '%s' not in 'data'", primary_key)
      }
      private$.cache = data.table(
        id = colnames(data),
        has_missings = replace(rep(NA, ncol(data)), ii, FALSE)
      )
    },

    #' @description
    #' Returns a slice of the data in the specified format.
    #' Currently, the only supported formats are `"data.table"` and `"Matrix"`.
    #' The rows must be addressed as vector of primary key values, columns must be referred to via column names.
    #' Queries for rows with no matching row id and queries for columns with no matching column name are silently ignored.
    #' Rows are guaranteed to be returned in the same order as `rows`, columns may be returned in an arbitrary order.
    #' Duplicated row ids result in duplicated rows, duplicated column names lead to an exception.
    data = function(rows, cols, data_format = "data.table") {
      rows = assert_integerish(rows, coerce = TRUE)
      assert_names(cols, type = "unique")
      assert_choice(data_format, self$data_formats)
      cols = intersect(cols, colnames(private$.data))

      if (self$compact_seq) {
        # https://github.com/Rdatatable/data.table/issues/3109
        rows = keep_in_bounds(rows, 1L, nrow(private$.data))
        data = private$.data[rows, cols, with = FALSE]
      } else {
        data = private$.data[list(rows), cols, with = FALSE, nomatch = NULL, on = self$primary_key]
      }
      return(data)
    },

    #' @description
    #' Retrieve the first `n` rows.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of rows.
    #'
    #' @return [data.table::data.table()] of the first `n` rows.
    head = function(n = 6L) {
      head(private$.data, n)
    },

    #' @description
    #' Returns a named list of vectors of distinct values for each column
    #' specified. If `na_rm` is `TRUE`, missing values are removed from the
    #' returned vectors of distinct values. Non-existing rows and columns are
    #' silently ignored.
    #'
    #' @return Named `list()` of distinct values.
    distinct = function(rows, cols, na_rm = TRUE) {
      cols = intersect(cols, colnames(private$.data))
      if (is.null(rows)) {
        set_names(lapply(cols, function(x) distinct_values(private$.data[[x]], drop = FALSE, na_rm = na_rm)), cols)
      } else {
        lapply(self$data(rows, cols), distinct_values, drop = TRUE, na_rm = na_rm)
      }
    },

    #' @description
    #' Returns the number of missing values per column in the specified slice
    #' of data. Non-existing rows and columns are silently ignored.
    #'
    #' @return Total of missing values per column (named `numeric()`).
    missings = function(rows, cols) {
      tab = private$.cache[list(cols), on = "id", nomatch = NULL]

      # update cache
      ii = tab[is.na(has_missings), which = TRUE]
      if (length(ii)) {
        tab[ii, has_missings := map_lgl(private$.data[, id, with = FALSE], anyMissing)]
        private$.cache = ujoin(private$.cache, tab[ii], key = "id")
      }

      # query required columns
      query_cols = tab[has_missings == TRUE, id]
      insert_named(
        named_vector(tab$id, 0L),
        map_int(self$data(rows = rows, cols = query_cols), count_missing)
      )
    }
  ),

  active = list(
    #' @field rownames (`integer()`)\cr
    #' Returns vector of all distinct row identifiers, i.e. the contents of the primary key column.
    rownames = function() {
      private$.data[[self$primary_key]]
    },

    #' @field colnames (`character()`)\cr
    #' Returns vector of all column names, including the primary key column.
    colnames = function() {
      colnames(private$.data)
    },

    #' @field nrow (`integer(1)`)\cr
    #' Number of rows (observations).
    nrow = function() {
      nrow(private$.data)
    },

    #' @field ncol (`integer(1)`)\cr
    #' Number of columns (variables), including the primary key column.
    ncol = function() {
      ncol(private$.data)
    }
  ),

  private = list(
    .calculate_hash = function() {
      hash(self$compact_seq, private$.data)
    },

    .cache = NULL
  )
)
