#' @title Class for Classif Learners
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct learners.
#'
#' @return [Learner()].
#' @family Learner
#' @include Learner.R
#' @export
LearnerClassif = R6Class("LearnerClassif",
  inherit = Learner,
  public = list(
    initialize = function(name, par_set = ParamSet$new(), par_vals = list(), packages = character(0L), properties = character(0L), train, predict) {
      super$initialize("classif", name, par_set, par_vals, packages, properties, train, predict, predict_type = "response")
    }
  )
)
