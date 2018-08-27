#' @title Base Class for Dictionaries
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} for a simple dictionary (hash map) storing R6 objects with slot \dQuote{id}.
#'
#' @field ids Returns the keys of registered items.
#' @field length Returns the number of registered items.
#' @field items Environment where all objects are stored.
#' @section Methods:
#' \describe{
#'  \item{\code{add(id, value)}}{...}
#'  \item{\code{get(id)}}{...}
#'  \item{\code{mget(ids)}}{...}
#'  \item{\code{remove(id)}}{...}
#' }
#'
#' @return [\code{Dictionary}].
Dictionary = R6Class("Dictionary",
  cloneable = FALSE,

  public = list(
    items = NULL,
    contains = NULL,

    # construct, set container type (string)
    initialize = function(contains) {
      self$contains = assertCharacter(contains, min.len = 1L, any.missing = FALSE, min.chars = 1L)
      self$items = new.env(parent = emptyenv())
    },

    add = function(value) {
      if (!inherits(value, "LazyValue"))
        assertClass(value, class = self$contains)
      assign(x = value$id, value = value, envir = self$items)
    },

    get = function(id, ...) {
      set_values = function(x, ...) {
        if (...length()) {
          dots = list(...)
          nn = names(dots)
          for (i in seq_along(dots)) {
            x[[nn[i]]] = dots[[i]]
          }
        }
        x
      }
      assert_string(id)
      if (!hasName(self$items, id))
        stopf("%s with id '%s' not found!", self$contains, id)
      x = private$retrieve(get(id, envir = self$items, inherits = FALSE))
      set_values(x, ...)
    },

    mget = function(ids) {
      assertCharacter(ids, any.missing = FALSE)
      missing = !hasName(self$items, ids)
      if (any(missing))
        stopf("%s with id '%s' not found!", self$contains, ids[wf(missing)])
      lapply(mget(ids, envir = self$items, inherits = FALSE), private$retrieve)
    },

    remove = function(id) {
      assert_string(id)
      if (!hasName(self$items, id))
        stopf("%s with id '%s' not found!", self$contains, id)
      rm(list = id, envir = self$items)
      invisible(self)
    }
  ),

  active = list(
    ids = function() ls(self$items, all.names = TRUE),
    length = function() length(self$items)
  ),

  private = list(
    retrieve = function(value) {
      if (inherits(value, "LazyValue")) value$getter() else value$clone()
    }
  )
)


LazyValue = function(id, getter) {
  obj = list(id = assert_string(id), getter = assertFunction(getter))
  class(obj) = "LazyValue"
  obj
}
