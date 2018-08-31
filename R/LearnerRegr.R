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
LearnerRegr = R6Class("LearnerRegr", inherit = Learner,
  private = list(
    .predict_type = "response"
  )
)
