BackendDataTable = R6Class("Backend",
  public = list(
    primary_key = NULL,

    initialize = function(data, primary_key = NULL) {
      assert_data_frame(data, min.rows = 1L, min.cols = 1L)

      if (is.null(primary_key)) {
        rn = attr(data, "row.names")
        data = as.data.table(data)
        if (is.character(rn))
          rn = make.unique(rn)
        data[["..row_id"]] = rn
        self$primary_key = "..row_id"
      } else {
        assert_string(primary_key)
        assert_names(colnames(data), must.include = primary_key)
        assert_atomic_vector(data[[primary_key]], any.missing = FALSE, unique = TRUE)
        self$primary_key = primary_key
        data = as.data.table(data)
      }
      private$dt = setkeyv(data, private$primary_key)
    },

    data = function(rows, cols) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique", subset.of = names(private$dt))

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
