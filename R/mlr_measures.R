#' Dictionary of registered performance measures
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @name mlr_measures
#' @examples
#' mlr_measures$ids
#' mlr_measures$get("mmce")
NULL

#' @export
mlr_measures = Dictionary$new("Measure")
