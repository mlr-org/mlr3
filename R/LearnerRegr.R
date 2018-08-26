#' @title Class for Regr Learners
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct learners.
#'
#' @return [\code{\link{Learner}}].
#' @family Learner
#' @include Learner.R
#' @export
LearnerRegr = R6Class("LearnerRegr",
  inherit = Learner,
  public = list(
    initialize = function(name, par.set = ParamSetFlat$new(), par.vals = list(), packages = character(0L), properties = character(0L), train, predict, model.extractors = list()) {
      super$initialize("regr", name, par.set, par.vals, packages, properties, train, predict, model.extractors, predict.type = "response")
    }
  )
)
