#' @title Dictionary of registered performance measures
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Measure
#' @name mlr_measures
#' @examples
#' mlr_measures$ids()
#' mlr_measures$get("mmce")
NULL

#' @include Dictionary.R
#' @export
mlr_measures = Dictionary$new("Measure")
