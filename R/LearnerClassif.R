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
LearnerClassif = R6Class("LearnerClassif", inherit = Learner,
  public = list(
    task_type = "classif"
  ),
  private = list(
    .predict_type = "response"
  )
)
