#' @title Cast objects using a Dictionary
#'
#' @description
#' Uses a dictionary to cast objects of a specific type.
#' Intended for package developers, internal use only.
#'
#' @param `x` (`character()` | `list()`)\cr
#'  Object to cast.
#' @param `type` (`character(1)`)\cr
#'  Expected type of objects.
#' @param `dict` ([Dictionary])\cr
#'  Expected type of objects.
#' @param `clone` (`logical(1)`)\cr
#'  Clone objects, if necessary. Default is `FALSE`.
#' @param `multiple` (`logical(1)`)\cr
#'  Cast multiple objects of type `type` or just a single one?
#'
#' @return Object of type `type` or list of objects of type `type`.
#' @keywords internal
#' @export
cast_from_dict = function(x, type, dict, clone = FALSE, multiple = TRUE) {
  if (inherits(x, type)) {
    if (clone)
      x = x$clone(deep = TRUE)
    return(list(x))
  }

  if (!is.character(x) && !is.list(x)) {
    stopf("Argument %s must be an object of type '%2$s', a list of elements of type '%2$s' or a character vector of keys to lookup in the dictionary",
      deparse(substitute(x)), type)
  }

  if (!multiple && length(x) != 1L) {
    stopf("Argument %s must be an object of type '%2$s', a list with one element of type '%2$s' or a single key to lookup in the dictionary",
      deparse(substitute(x)), type)
  }

  map(x, function(xi) {
    if (inherits(xi, type))
      return(if (clone) xi$clone(deep = TRUE) else xi)
    if (is.character(xi) && length(xi) == 1L)
      return(dict$get(xi))

    stopf("Argument %s must be an object of type '%2$s', a list of elements of type '%2$s' or a character vector of keys to lookup in the dictionary",
      deparse(substitute(x)), type)
  })
}
