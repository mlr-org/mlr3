Cache = R6Class("Cache",
  public = list(
    cache = NULL,
    initialize = function() {
      self$cache = new.env(parent = emptyenv())
    },

    remember = function(key, value) {
      assign(key, value, envir = self$cache)
      invisible(self)
    },

    forget = function(keys = NULL) {
      if (is.null(keys)) {
        self$cache = new.env(parent = emptyenv())
      } else {
        x = intersect(keys, ls(self$cache, all.names = TRUE))
        rm(list = x, envir = self$cache)
      }
      invisible(self)
    },

    recall = function(key, expr) {
      if (hasName(self$cache, key)) {
        return(self$cache[[key]])
      }
      self$remember(key, expr)
      return(expr)
    }
  )
)
