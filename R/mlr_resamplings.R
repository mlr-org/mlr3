#' Dictionary of registered resampling strategies
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @name mlr_resamplings
#' @family Dictionary
#' @examples
#' mlr_resamplings$ids
#' mlr_resamplings$get("cv")
NULL

#' @export
mlr_resamplings = Dictionary$new("Resampling")
