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
    initialize = function(id, feature_types = character(0L), predict_types = "response", packages = character(0L), par_set = ParamSet$new(), par_vals = list(), properties = character(0L)) {
      super$initialize(id = id, task_type = "regr", feature_types = feature_types, predict_types = predict_types, packages = packages,
        par_set = par_set, par_vals = par_vals, properties = properties)
      assert_subset(self$properties, mlr_reflections$learner_properties$regr)
      private$.predict_type = predict_types[1L]
    }
  )
)
