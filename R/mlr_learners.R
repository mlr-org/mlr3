#' @title Dictionary of registered learners
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Learner
#' @name mlr_learners
#' @examples
#' mlr_learners$ids
#' mlr_learners$get("classif.dummy")
NULL

#' @include Dictionary.R
#' @export
mlr_learners = Dictionary$new("Learner")
