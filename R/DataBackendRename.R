#' @include DataBackend.R
DataBackendRename = R6Class("DataBackendRename", inherit = DataBackend, cloneable = FALSE,
  public = list(
    old = NULL,
    new = NULL,

    initialize = function(b, old, new) {
      super$initialize(data = b, b$primary_key, "data.table")
      assert_character(old, any.missing = FALSE, unique = TRUE)
      assert_subset(old, b$colnames)
      assert_character(new, any.missing = FALSE, len = length(old))
      assert_names(new, if (allow_utf8_names()) "unique" else "strict")

      ii = old != new
      self$old = old[ii]
      self$new = new[ii]
    },

    data = function(rows, cols, data_format = self$data_formats[1L]) {
      assert_names(cols, type = "unique")
      b = private$.data
      cols = map_values(intersect(cols, self$colnames), self$new, self$old)
      data = b$data(rows, cols, data_format)
      set_col_names(data, map_values(names(data), self$old, self$new))
    },

    head = function(n = 6L) {
      data = private$.data$head(n)
      set_col_names(data, map_values(names(data), self$old, self$new))
    },

    distinct = function(rows, cols, na_rm = TRUE) {
      cols = map_values(intersect(cols, self$colnames), self$new, self$old)
      x = private$.data$distinct(rows, cols, na_rm = na_rm)
      set_names(x, map_values(names(x), self$old, self$new))
    },

    missings = function(rows, cols) {
      cols = map_values(intersect(cols, self$colnames), self$new, self$old)
      x = private$.data$missings(rows, cols)
      set_names(x, map_values(names(x), self$old, self$new))
    }
  ),

  active = list(
    rownames = function() {
      private$.data$rownames
    },

    colnames = function() {
      x = private$.data$colnames
      map_values(x, self$old, self$new)
    },

    nrow = function() {
      private$.data$nrow
    },

    ncol = function() {
      private$.data$ncol
    }
  ),

  private = list(
    .calculate_hash = function() {
      hash(self$old, self$new, private$.data$hash)
    }
  )
)
