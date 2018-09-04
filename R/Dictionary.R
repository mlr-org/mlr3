#' @title Key-value storage
#'
#' @description
#' A simple key-value store for \pkg{R6} objects with support of lazy-loading objects.
#'
#' @section Usage:
#' ```
#' d = Dictionary$new(contains)
#'
#' d$add(value)
#' d$get(id)
#' d$mget(ids)
#' d$remove(ids)
#' d$ids
#' ```
#'
#' @section Arguments:
#' * `contains` (`string`):
#'   Class of objects to store. Used for assertions.
#' * `id` (`string`):
#'   Key of the object to work on.
#' * `ids` (`string`):
#'   Keys of the objects to work on.
#'
#' @section Details:
#' `$new()` initializes a new object of class [Dictionary].
#'
#' `$get()` retrieves a single object with key `id` (or raises an exception).
#'
#' `$mget()` retrieves a named list of objects with keys `ids` (or raises an exception).
#'
#' `$remove()` removes item with id `id` from the Dictionary.
#'
#' `$ids` returns a vector of type `character` with all ids.
#'
#' @name Dictionary
#' @family Dictionary
#' @keywords internal
NULL

#' @export
Dictionary = R6Class("Dictionary",
  cloneable = FALSE,

  public = list(
    items = NULL,
    contains = NULL,

    # construct, set container type (string)
    initialize = function(contains) {
      self$contains = assert_character(contains, min.len = 1L, any.missing = FALSE, min.chars = 1L)
      self$items = new.env(parent = emptyenv())
    },

    add = function(value, id = value$id) {
      assert_id(id)
      if (!inherits(value, "LazyValue"))
        assert_class(value, class = self$contains)
      assign(x = id, value = value, envir = self$items)
    },

    get = function(id, ...) {
      assert_id(id)
      if (!hasName(self$items, id))
        stopf("%s with id '%s' not found!", self$contains, id)
      private$retrieve(get(id, envir = self$items, inherits = FALSE))
    },

    mget = function(ids) {
      assert_character(ids, any.missing = FALSE)
      missing = !hasName(self$items, ids)
      if (any(missing))
        stopf("%s with id '%s' not found!", self$contains, ids[wf(missing)])
      lapply(mget(ids, envir = self$items, inherits = FALSE), private$retrieve)
    },

    remove = function(id) {
      assert_id(id)
      if (!hasName(self$items, id))
        stopf("%s with id '%s' not found!", self$contains, id)
      rm(list = id, envir = self$items)
      invisible(self)
    }
  ),

  active = list(
    ids = function() ls(self$items, all.names = TRUE)
  ),

  private = list(
    retrieve = function(value) {
      if (inherits(value, "LazyValue")) value$getter() else value$clone()
    }
  )
)


LazyValue = function(id, getter) {
  obj = list(id = assert_id(id), getter = assert_function(getter))
  class(obj) = "LazyValue"
  obj
}
