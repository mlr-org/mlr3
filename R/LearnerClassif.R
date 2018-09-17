#' @title Class for classification learners
#'
#' @description
#' Class for classification learners. Inherits from [Learner].
#' Predict type is set to "response" by default.
#'
#' Instantiated learners can be retrieved from [mlr_learners].
#'
#' @section Usage:
#' ```
#' l = LearnerClassif$new()
#' ```
#'
#' @name LearnerClassif
#' @family Learner
NULL

#' @export
LearnerClassif = R6Class("LearnerClassif", inherit = Learner,
  public = list(
    task_type = "classif",
    initialize = function(id, packages = character(0L), par_set = ParamSet$new(), par_vals = list(), properties = character(0L)) {
      super$initialize(id = id, packages = packages, par_set = par_set, par_vals = par_vals, properties = properties)
      assert_subset(self$properties, capabilities$learner_props$classif)
    }
  ),

  private = list(
    .predict_type = "response"
  )
)
