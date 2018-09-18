#' @title Class for regression learners
#'
#' @description
#' Class for regression learners. Inherits from [Learner].
#' Predict type is set to "response" by default.
#'
#' Instantiated learners can be retrieved from [mlr_learners].
#'
#' @section Usage:
#' ```
#' l = LearnerRegr$new()
#' ```
#'
#' @name LearnerRegr
#' @family Learner
NULL

#' @export
LearnerRegr = R6Class("LearnerRegr", inherit = Learner,
  public = list(
    task_type = "regr",
    initialize = function(id, packages = character(0L), par_set = ParamSet$new(), par_vals = list(), properties = character(0L)) {
      super$initialize(id = id, packages = packages, par_set = par_set, par_vals = par_vals, properties = properties)
      assert_subset(self$properties, capabilities$learner_props$regr)
    }
  ),
  private = list(
    .predict_type = "response"
  )
)
