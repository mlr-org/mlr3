#' @include DataBackend.R
DataBackendRename = R6Class("DataBackendRename", inherit = DataBackend, cloneable = FALSE,
  public = list(
    map = NULL,
    initialize = function(b, map) {
      super$initialize(data = b, b$primary_key, "data.table")
      assert_character(map)
      assert_names(names(map), subset.of = b$colnames)
      assert_names(map, "strict")
      self$map = map
    },

    data = function(rows, cols, data_format = self$data_formats[1L]) {
      assert_atomic_vector(rows)
      assert_names(cols, type = "unique")
      b = private$.data
      cols = map_values(intersect(cols, self$colnames), self$map, names(self$map))
      data = b$data(rows, cols, data_format)
      set_col_names(data, map_values(names(data), names(self$map), self$map))
    },

    head = function(n = 6L) {
      data = private$.data$head(n)
      set_col_names(data, map_values(names(data), names(self$map), self$map))
    },

    distinct = function(rows, cols) {
      cols = map_values(intersect(cols, self$colnames), self$map, names(self$map))
      x = private$.data$distinct(rows, cols)
      set_names(x, map_values(names(x), names(self$map), self$map))
    },

    missings = function(rows, cols) {
      cols = map_values(intersect(cols, self$colnames), self$map, names(self$map))
      x = private$.data$missings(rows, cols)
      set_names(x, map_values(names(x), names(self$map), self$map))
    }
  ),

  active = list(
    rownames = function() {
      private$.data$rownames
    },

    colnames = function() {
      x = private$.data$colnames
      map_values(x, names(self$map), self$map)
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
      private$.data$hash
    }
  )
)
