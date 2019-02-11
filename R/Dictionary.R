#' @title Key-Value Storage
#'
#' @name Dictionary
#' @format [R6::R6Class] object.
#' @description
#' A simple key-value store for [R6::R6] generator objects.
#' On retrieval of an object, the following applies:
#'
#' * R6 Factories (objects of class `R6ClassGenerator`) are initialized (with additional arguments).
#' * Functions are called (with additional arguments) and must return an instance of a [R6::R6] object.
#'
#' @section Usage:
#' ```
#' # Construction
#' d = Dictionary$new()
#'
#' # Methods
#' d$add(value)
#' d$get(key, ...)
#' d$has(keys)
#' d$keys(pattern)
#' d$mget(keys, ...)
#' d$remove(key, value)
#' d$remove(keys)
#'
#' ## S3 methods
#' as.data.frame(d)
#' as.data.table(d)
#' ```
#'
#' @section Arguments:
#' * `pattern` (`character(1)`): Restrict to ids which match the regular expression `pattern`.
#' * `id` (`character(1)`): Single id as string.
#' * `value`: Arbitrary value.
#' * `ids` (`character()`): Vector of ids.
#'
#' @section Details:
#' * `$add()` adds item `value` with key `key` to the Dictionary.
#' * `$get()` retrieves a single object with key `key` (or raises an exception).
#' * `$has()` (`logical()`) returns a named logical of the same length as `keys` with value `TRUE` if the respective key is found in the Dictionary.
#' * `$keys()` (`character()`) returns a vector with all keys (or all keys matching `pattern`).
#' * `$mget()` (named `list`) creates a list of objects with keys `keys` (or raises an exception).
#' * `$new()` initializes a new object of class [Dictionary].
#' * `$remove()` removes item with key `key` from the Dictionary.
#' * `as.data.frame()` and `as.data.table()` give a summarizing overview as [data.frame()] or [data.table()], respectively.#'
#'
#' @keywords internal
#' @family Dictionary
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

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      ids = self$ids()
      catf(sprintf("%s with %i stored values", format(self), length(ids)))
      catf(str_indent("Ids:", ids))

      catf(str_indent("\nPublic: ", str_r6_interface(self)))
    },

    ids = function(pattern = NULL) {
      ids = ls(self$items, all.names = TRUE)
      if (!is.null(pattern))
        ids = ids[grepl(assert_string(pattern), ids)]
      ids
    },

    has = function(ids) {
      assert_character(ids, any.missing = FALSE)
      set_names(map_lgl(ids, exists, envir = self$items, inherits = FALSE), ids)
    },

    add = function(id, value) {
      assert_id(id)
      assign(x = id, value = value, envir = self$items)
      invisible(self)
    },

    remove = function(id) {
      if (id %nin% self$ids())
        stopf("Element with key '%s' not found!%s", id, did_you_mean(id, self$ids()))
      rm(list = id, envir = self$items)
      invisible(self)
    },

    get = function(key, ...) {
      dictionary_retrieve(self, key, ...)
    },

    mget = function(keys, ...) {
      set_names(lapply(keys, self$get, ...), keys)
    }
  )
)

dictionary_retrieve = function(self, key, ...) {
  value = get0(key, envir = self$items, inherits = FALSE, ifnotfound = NULL)
  if (is.null(value))
    stopf("Element with key '%s' not found!%s", key, did_you_mean(key, self$ids()))

  if (inherits(value, "R6ClassGenerator")) {
    value = value$new(...)
  } else if (is.function(value)) {
    value = assert_r6(value(...))
  }
  return(value)
}

#' @export
as.data.table.Dictionary = function(x, ...) {
  data.table(id = x$ids())
}

#' @export
as.data.frame.Dictionary = function(x, ...) {
  setDF(as.data.table(x))[]
}
