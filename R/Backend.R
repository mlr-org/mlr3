BackendDataTable = R6Class("Backend",
  public = list(
    primary_key = NULL,

    initialize = function(data, primary_key = NULL) {
      assertDataFrame(data, min.rows = 1L, min.cols = 1L)

      if (is.null(primary_key)) {
        rn = attr(data, "row.names")
        data = as.data.table(data)
        if (is.character(rn))
          rn = make.unique(rn)
        data[["..row.id"]] = rn
        self$primary_key = "..row.id"
      } else {
        assertString(primary_key)
        assertNames(colnames(data), must.include = primary_key)
        assertAtomicVector(data[[primary_key]], any.missing = FALSE, unique = TRUE)
        self$primary_key = primary_key
        data = as.data.table(data)
      }
      private$dt = setkeyv(data, private$primary_key)
    },

    data = function(rows, cols) {
      assertAtomicVector(rows)
      assertNames(cols, type = "unique", subset.of = names(private$dt))

      data = private$dt[list(rows), cols, with = FALSE, nomatch = 0L, on = self$primary_key]
      return(data)
    },

    head = function(n = 6L) {
      head(private$dt, n)
    }
  ),

  active = list(
    colnames = function() {
      colnames(private$dt)
    },

    rownames = function() {
      private$dt[[self$primary_key]]
    },

    nrow = function() {
      nrow(private$dt)
    },

    ncol = function() {
      ncol(private$dt)
    }
  ),

  private = list(
    dt = NULL,

    deep_clone = function(name, value) {
      if (name == "dt") copy(name) else value
    }
  )
)

#' @export
as.data.table.Backend = function(x) {
  x$data(x$rownames, x$colnames)
}
