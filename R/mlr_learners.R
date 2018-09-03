#' Dictionary of registered learners
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @name mlr_learners
#' @examples
#' mlr_learners$ids
#' mlr_learners$get("classif.dummy")
NULL

#' @export
mlr_learners = Dictionary$new("Learner")
