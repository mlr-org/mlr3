#' @name LearnerClassif
#' @rdname Learner
NULL

#' @export
LearnerClassif = R6Class("LearnerClassif", inherit = Learner,
  public = list(
    initialize = function(id, feature_types = character(0L), predict_types = "response", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L)) {
      super$initialize(id = id, task_type = "classif", feature_types = feature_types, predict_types = predict_types, packages = packages,
        param_set = param_set, param_vals = param_vals, properties = properties)
      assert_subset(self$properties, mlr_reflections$learner_properties$classif)
      private$.predict_type = predict_types[1L]
    }
  )
)
