#' @title Key-Value Storage
#'
#' @description
#' A simple key-value store for [R6::R6] objects.
#' On retrieval of an object, the following applies:
#'
#' * R6 Factories (objects of class `R6ClassGenerator`) are initialized (with no arguments).
#' * [R6::R6Class] objects are cloned.
#' * Functions are called (with no arguments).
#' * All other objects are returned as-is.
#'
#' @section Usage:
#' ```
#' # Construction
#' d = Dictionary$new()
#' # Getters
#' d$keys(pattern)
#' d$add(value)
#' d$get(key)
#' d$mget(keys)
#' d$has(key)
#' d$remove(key, value)
#' d$remove(keys)
#' # S3 methods
#' as.data.frame(d)
#' as.data.table(d)
#' ```
#'
#' @section Arguments:
#' * `pattern` (`character(1)`):\cr
#'  Restrict keys to keys  which match `pattern`.
#' * `key` (`character(1)`):\cr
#'   Single Key as string.
#' * `value`:\cr
#'   Arbitrary value.
#' * `keys` (`character()`):\cr
#'   Vector of keys.
#'
#' @section Details:
#' * `$new()` initializes a new object of class [Dictionary].
#' * `$keys()` (`character()`) returns a vector with all keys (or all keys matching `pattern`).
#' * `$get()` retrieves a single object with key `key` (or raises an exception).
#' * `$mget()` (named `list`) creates a list of objects with keys `keys` (or raises an exception).
#' * `$has()` (`logical()`) is `TRUE` if `key` is present in the Dictionary.
#' * `$add()` adds item `value` with key `key` to the Dictionary.
#' * `$remove()` removes item with key `key` from the Dictionary.
#' * `as.data.frame()` and `as.data.table()` give a summarizing overview as `data.frame` or `data.table`, respectively.
#'
#' @keywords internal
#' @name Dictionary
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

    print = function(...) {
      keys = self$keys()
      catf(str_indent(sprintf("<%s> with %i stored values:", class(self)[1L], length(keys)), keys))
      catf(str_indent("\nPublic: ", str_r6_interface(self)))
    },

    keys = function(pattern = NULL) {
      keys = ls(self$items, all.names = TRUE)
      if (!is.null(pattern))
        keys = keys[grepl(assert_string(pattern), keys)]
      keys
    },

    has = function(key) {
      assert_id(key)
      exists(key, envir = self$items, inherits = FALSE)
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
      set_names(lapply(keys, dictionary_retrieve, self = self), keys)
    }
  )
)

assert_keys_exist = function(x, dict) {
  keys = ls(dict$items, all.names = TRUE)
  ii = wf(x %nin% keys)
  if (length(ii) > 0L)
    stopf("Element with key '%s' not found!%s", x[ii], did_you_mean(x[ii], keys))
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

#' @export
as.data.table.Dictionary = function(x, ...) {
  data.table(id = x$keys())
}

#' @export
as.data.frame.Dictionary = function(x, ...) {
  setDF(as.data.table(x))[]
}
