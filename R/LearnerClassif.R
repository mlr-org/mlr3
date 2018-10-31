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
    initialize = function(id, feature_types = character(0L), predict_types = "response", packages = character(0L), par_set = ParamSet$new(), par_vals = list(), properties = character(0L)) {
      super$initialize(id = id, task_type = "classif", feature_types = feature_types, predict_types = predict_types, packages = packages,
        par_set = par_set, par_vals = par_vals, properties = properties)
      assert_subset(self$properties, mlr_reflections$learner_properties$classif)
      private$.predict_type = predict_types[1L]
    }
  )
)
