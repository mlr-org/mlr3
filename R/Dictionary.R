#' @title Key-value storage
#'
#' @description
#' A simple key-value store for \pkg{R6} objects.
#' On retrieval of an object, the following applies:
#'
#' * Factories (objects of class `R6ClassGenerator`) are initialized.
#' * R6 objects are cloned.
#' * Functions called.
#' * All other objects are returned as-is.
#'
#' @section Usage:
#' ```
#' d = Dictionary$new()
#'
#' d$keys(pattern)
#' d$add(value)
#' d$get(key)
#' d$mget(keys)
#' d$remove(keys)
#' ```
#'
#' @section Arguments:
#' * `pattern` (`string`):
#'  Restrict keys to keys  which match the `pattern`.
#' * `key` (`string`):
#'   Key of single object to work on.
#' * `keys` (`string`):
#'   Keys of multiple objects to work on.
#'
#' @section Details:
#' `$new()` initializes a new object of class [Dictionary].
#'
#' `$keys()` returns a vector of type `character` with all keys (or all keys matching `pattern`).
#'
#' `$get()` retrieves a single object with key `key` (or raises an exception).
#'
#' `$mget()` retrieves a named list of objects with keys `keys` (or raises an exception).
#'
#' `$remove()` removes item with key `key` from the Dictionary.
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

    # construct, set container type (string)
    initialize = function() {
      self$items = new.env(parent = emptyenv())
    },

    print = function(...) {
      keys = self$keys()
      catf(stri_wrap(initial = sprintf("<%s> with %i stored values: ", class(self)[1L], length(keys)), keys))
      catf(stri_wrap(initial = "\nPublic: ", setdiff(ls(self), c("initialize", "print"))))
    },

    keys = function(pattern = NULL) {
      keys = ls(self$items, all.names = TRUE)
      if (!is.null(pattern))
        keys = keys[grepl(assert_string(pattern), keys)]
      keys
    },

    add = function(key, value) {
      assert_id(key)
      assign(x = key, value = value, envir = self$items)
      invisible(self)
    },

    remove = function(key) {
      assert_keys_exist(assert_id(key), self)
      rm(list = key, envir = self$items)
      invisible(self)
    },

    get = function(key) {
      assert_keys_exist(assert_id(key), self)
      dictionary_retrieve(self, key)
    },

    mget = function(keys) {
      assert_keys_exist(assert_character(keys, any.missing = FALSE), self)
      setNames(lapply(keys, dictionary_retrieve, self = self), keys)
    },

    has = function(key) {
      assert_id(key)
      exists(key, envir = self$items, inherits = FALSE)
    }
  )
)

assert_keys_exist = function(x, dict) {
  keys = ls(dict$items, all.names = TRUE)
  ii = wf(x %nin% keys)
  if (length(ii) > 0L) {
    suggested = stri_suggest(x[ii], keys)
    suggested = if (length(suggested) == 0L) "" else sprintf(" Did you mean: %s?", paste0(suggested, collapse = " / "))
    stopf("Element %s with key '%s' not found!%s", x[ii], suggested)
  }
  x
}

dictionary_retrieve = function(self, key) {
  value = get(key, envir = self$items, inherits = FALSE)
  if (inherits(value, "R6ClassGenerator")) {
    instance = value$new()
    assign(key, instance, envir = self$items)
    value = instance$clone()
    return(instance$clone())
  } else if (inherits(value, "R6")) {
    value = value$clone()
  } else if (is.function(value)) {
    value = value()
  }
  return(value)
}
