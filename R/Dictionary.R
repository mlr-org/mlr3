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
#' d$ids(pattern)
#' d$add(value)
#' d$get(id)
#' d$mget(ids)
#' d$has(ids)
#' d$remove(id, value)
#' d$remove(ids)
#' # S3 methods
#' as.data.frame(d)
#' as.data.table(d)
#' ```
#'
#' @section Arguments:
#' * `pattern` (`character(1)`):\cr
#'  Restrict to ids which match the regular expression `pattern`.
#' * `id` (`character(1)`):\cr
#'   Single id as string.
#' * `value`:\cr
#'   Arbitrary value.
#' * `ids` (`character()`):\cr
#'   Vector of ids.
#'
#' @section Details:
#' * `$new()` initializes a new object of class [Dictionary].
#' * `$ids()` (`character()`) returns a vector with all ids (or all ids matching `pattern`).
#' * `$get()` retrieves a single object with id `id` (or raises an exception).
#' * `$mget()` (named `list`) creates a list of objects with ids `ids` (or raises an exception).
#' * `$has()` (`logical()`) returns a named logical of the same length as `ids` with value `TRUE` if the respective id is found in the Dictionary.
#' * `$add()` adds item `value` with id `id` to the Dictionary.
#' * `$remove()` removes item with id `id` from the Dictionary.
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
      assert_ids_exist(assert_id(id), self)
      rm(list = id, envir = self$items)
      invisible(self)
    },

    get = function(id) {
      assert_ids_exist(assert_id(id), self)
      dictionary_retrieve(self, id)
    },

    mget = function(ids) {
      assert_ids_exist(assert_character(ids, any.missing = FALSE), self)
      set_names(lapply(ids, dictionary_retrieve, self = self), ids)
    }
  )
)

assert_ids_exist = function(x, dict) {
  ids = ls(dict$items, all.names = TRUE)
  ii = wf(x %nin% ids)
  if (length(ii) > 0L)
    stopf("Element with key '%s' not found!%s", x[ii], did_you_mean(x[ii], ids))
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
  data.table(id = x$ids())
}

#' @export
as.data.frame.Dictionary = function(x, ...) {
  setDF(as.data.table(x))[]
}

