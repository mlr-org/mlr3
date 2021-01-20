# nolint start

#' @include mlr_reflections.R
mlr_reflections$auto_converters = ee = new.env(parent = emptyenv())

## from: logical
ee[["logical___integer"]] =
  function(value, type, levels) { as.integer(value) }
ee[["logical___numeric"]] =
  function(value, type, levels) { as.double(value) }
ee[["logical___character"]] =
  function(value, type, levels) { as.character(value) }
ee[["logical___factor"]] =
  function(value, type, levels) { factor(value, levels = union(levels, value), ordered = FALSE) }
ee[["logical___ordered"]] =
  function(value, type, levels) { if (all(value %in% c(NA_character_, levels))) factor(value, levels = levels, ordered = TRUE) else NULL }

## from: integer
ee[["integer___logical"]] =
  function(value, type, levels) { if (test_integerish(value, lower = 0L, upper = 1L)) as.logical(value) else value }
ee[["integer___numeric"]] =
  ee[["logical___numeric"]]
ee[["integer___character"]] =
  ee[["logical___character"]]
ee[["integer___factor"]] =
  ee[["logical___factor"]]
ee[["integer___ordered"]] =
  ee[["logical___ordered"]]

## from: numeric
ee[["numeric___logical"]] =
  ee[["integer___logical"]]
ee[["numeric___integer"]] =
  function(value, type, levels) { if (test_integerish(value)) as.integer(value) else value }
ee[["numeric___character"]] =
  ee[["logical___character"]]
ee[["numeric___factor"]] =
  ee[["logical___factor"]]
ee[["numeric___ordered"]] =
  ee[["logical___ordered"]]

## from: character
ee[["character___logical"]] =
  function(value, type, levels) { if (all(value %in% c(NA_character_, "TRUE", "FALSE", "true", "false", "T", "F"))) as.logical(value) else value }
ee[["character___integer"]] =
  function(value, type, levels) { if (allMissing(value)) as.integer(value) else value }
ee[["character___numeric"]] =
  function(value, type, levels) { if (allMissing(value)) as.double(value) else value }
ee[["character___factor"]] =
  ee[["logical___factor"]]
ee[["character___ordered"]] =
  ee[["logical___ordered"]]

## from: factor
ee[["factor___logical"]] =
  ee[["character___logical"]]
ee[["factor___character"]] =
  ee[["logical___character"]]
ee[["factor___factor"]] =
  function(value, type, levels) { factor(value, levels = union(levels, levels(value)), ordered = FALSE) }
ee[["factor___ordered"]] =
  function(value, type, levels) { if (all(levels(value) %in% levels)) factor(value, levels = levels, ordered = TRUE) else NULL }

## from: ordered
ee[["ordered___character"]] =
  ee[["logical___character"]]
ee[["ordered___factor"]] =
  ee[["factor___factor"]]
ee[["ordered___ordered"]] =
  ee[["ordered___ordered"]]

rm(ee)
# nolint end

#' @title Column Auto-Converter
#'
#' @description
#' Set of rules to automatically convert column types.
#' This is used during `rbind`-ing of [Task]s, but also in some pipe operators in
#' \CRANpkg{mlr3pipelines}.
#'
#' All rules are stored as functions in [mlr_reflections$auto_converters][mlr_reflections].
#'
#' @param value (`any`)\cr
#'   New values to convert in order to match `type`.
#' @param id (`character(1)`)\cr
#'   Name of the column, used in error messages.
#' @param type (`character(1)`)\cr
#'   Type to convert `values` to.
#' @param levels (`character()` | `NULL`)\cr
#'   Levels to use for conversion to `factor` or `ordered`.
#'
#' @return Vector `value` converted to type `type`.
#'
#' @keywords internal
#' @export
auto_convert = function(value, id, type, levels) {
  cl = class(value)[1L]
  fun = get0(sprintf("%s___%s", cl, type), envir = mlr_reflections$auto_converters, inherits = FALSE)

  if (!is.null(fun)) {
    value = fun(value, type, levels)
  }

  if (class(value)[1L] != type) {
    stopf("Incompatible types during auto-converting column '%s': failed to convert from class '%s' to class '%s'", id, cl, type)
  }

  value
}
