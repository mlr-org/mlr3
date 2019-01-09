#' @include DataBackend.R
DataBackendReplaceColumns = R6Class("DataBackendReplaceColumns", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2) {
      assert_backend(b1)
      assert_backend(b2)
      if (b1$primary_key != b2$primary_key)
        stopf("All backends must have the same primary_key")
      super$initialize(list(b1 = b1, b2 = b2), b1$primary_key, intersect(b1$formats, b2$formats))
    },

    data = function(rows, cols, format = self$formats[1L]) {
      assert_choice(format, self$formats)
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      cols = private$.split_cols(cols)

      x = private$.data$b1$data(rows, union(cols$x, self$primary_key), format)
      y = private$.data$b2$data(rows, cols$y, format)
      insert_named(x, y)[, cols$cols, with = FALSE]
    },

    head = function(n = 6L) {
      x = private$.data$b1$head(n)
      y = private$.data$b2$data(rows = x[[self$primary_key]], cols = setdiff(private$.data$b2$colnames, self$primary_key))
      insert_named(x, y)
    },

    distinct = function(cols) {
      cols = private$.split_cols(cols)
      c(private$.data$b1$distinct(cols$x), private$.data$b2$distinct(cols$y))[cols$cols]
    },

    missing = function(rows, cols) {
      cols = private$.split_cols(cols)
      c(private$.data$b1$missing(rows, cols$x), private$.data$b2$missing(rows, cols$y))[cols$cols]
    }
  ),

  active = list(
    rownames = function() {
      private$.data$b1$rownames
    },

    colnames = function() {
      private$.data$b1$colnames
    },

    nrow = function() {
      private$.data$b1$nrow
    },

    ncol = function() {
      private$.data$b1$ncol
    }
  ),

  private = list(
    .split_cols = function(cols) {
      cols = intersect(cols, self$colnames)
      replaced_cols = setdiff(private$.data$b2$colnames, self$primary_key)
      list(cols = cols, x = setdiff(cols, replaced_cols), y = intersect(cols, replaced_cols))
    }
  )
)
