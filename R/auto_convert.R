#' @include mlr_reflections.R
mlr_reflections$auto_converters = AC = new.env(parent = emptyenv())

## from: logical
AC[["logical___integer"]] = function(value, id, type, levels) { as.integer(value) }
AC[["logical___numeric"]] = function(value, id, type, levels) { as.double(value) }
AC[["logical___character"]] = function(value, id, type, levels) { as.character(value) }
AC[["logical___factor"]] = function(value, id, type, levels) { as_factor(value, levels = union(levels, value), ordered = FALSE) }
AC[["logical___ordered"]] = function(value, id, type, levels) { if (all(value %in% c(levels, NA))) factor(value, levels = levels, ordered = TRUE) else NULL }

## from: integer
AC[["integer___logical"]] = function(value, id, type, levels) { if (test_integerish(value, lower = 0L, upper = 1L)) as.logical(value) else value }
AC[["integer___numeric"]] = AC[["logical___numeric"]]
AC[["integer___character"]] = AC[["logical___character"]]
AC[["integer___factor"]] = AC[["logical___factor"]]
AC[["integer___ordered"]] = AC[["logical___ordered"]]

## from: numeric
AC[["numeric___logical"]] = AC[["integer___logical"]]
AC[["numeric___integer"]] = function(value, id, type, levels) { if (test_integerish(value)) as.integer(value) else value }
AC[["numeric___character"]] = AC[["logical___character"]]
AC[["numeric___factor"]] = AC[["logical___factor"]]
AC[["numeric___ordered"]] = AC[["logical___ordered"]]

## from: character
AC[["character___logical"]] = function(value, id, type, levels) { if (all(is.na(value) | tolower(value) %in% c("true", "false", "t", "f"))) as.logical(value) else value }
AC[["character___integer"]] = function(value, id, type, levels) { if (allMissing(value)) as.integer(value) else value }
AC[["character___numeric"]] = function(value, id, type, levels) { if (allMissing(value)) as.double(value) else value }
AC[["character___factor"]] = AC[["logical___factor"]]
AC[["character___ordered"]] = AC[["logical___ordered"]]

## from: factor
AC[["factor___character"]] = AC[["logical___character"]]
AC[["factor___factor"]] = AC[["logical___factor"]]
AC[["factor___ordered"]] = AC[["logical___ordered"]]

## from: ordered
AC[["ordered___character"]] = AC[["logical___character"]]
AC[["ordered___factor"]] = AC[["logical___factor"]]
AC[["ordered___ordered"]] = AC[["logical___ordered"]]


auto_convert = function(value, id, type, levels) {
  cl = class(value)[1L]
  fun = get0(sprintf("%s___%s", cl, type), envir = AC, inherits = FALSE)

  if (!is.null(fun)) {
    value = fun(value, id, type, levels)
  }

  if (class(value)[1L] != type) {
    stopf("Incompatible types during auto-convert from class '%s' to class '%s'", cl, type)
  }
  value
}
