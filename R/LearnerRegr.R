#' @title Regression Learner
#' @format [R6Class] object
#' @description
#' This Learner specializes [Learner] for regression problems.
#'
#' @section Usage:
#' See [Learner].
#'
#' @section Details:
#' * `$task_type` is `"regr"`.
#'
#' @name LearnerRegr
#' @family Learner
NULL

#' @export
LearnerRegr = R6Class("LearnerRegr", inherit = Learner,
  public = list(
    initialize = function(id, feature_types = character(0L), predict_types = "response", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L)) {
      super$initialize(id = id, task_type = "regr", feature_types = feature_types, predict_types = predict_types, packages = packages,
        param_set = param_set, param_vals = param_vals, properties = properties)
      assert_subset(self$properties, mlr_reflections$learner_properties$regr)
      private$.predict_type = predict_types[1L]
    }
  )
)
