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
    task_type = "TaskClassif",
    initialize = function(id, packages = character(0L), par_set = ParamSet$new(), properties = character(0L)) {
      super$initialize(id = id, packages = packages, par_set = par_set, properties = properties)
    }
  ),
  private = list(
    .predict_type = "response"
  )
)
