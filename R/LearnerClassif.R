#' @title Class for Classif Learners
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct learners.
#'
#' @return [\code{\link{Learner}}].
#' @family Learner
#' @include Learner.R
#' @export
LearnerClassif = R6Class("LearnerClassif",
  inherit = Learner,
  public = list(
    initialize = function(name, par_set = ParamSetFlat$new(), par_vals = list(), packages = character(0L), properties = character(0L), train, predict) {
      super$initialize("classif", name, par_set, par_vals, packages, properties, train, predict, predict_type = "response")
    }
  )
)
