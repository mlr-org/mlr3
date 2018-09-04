#' Dictionary of registered resampling strategies
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @name mlr_resamplings
#' @family Dictionary
#' @family Resampling
#' @examples
#' mlr_resamplings$ids
#' mlr_resamplings$get("cv")
NULL

#' @include Dictionary.R
#' @export
mlr_resamplings = Dictionary$new("Resampling")
