#' @title Class for Regr Learners
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct learners.
#'
#' @return [Learner()].
#' @family Learner
#' @include Learner.R
#' @export
LearnerRegr = R6Class("LearnerRegr",
  inherit = Learner,
  public = list(
    initialize = function(name, par_set = ParamSetFlat$new(), par_vals = list(), packages = character(0L), properties = character(0L), train, predict) {
      super$initialize("regr", name, par_set, par_vals, packages, properties, train, predict, predict_type = "response")
    }
  )
)
