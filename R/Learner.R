#' @title Learner Class
#'
#' @description
#' Predefined learners are stored in [mlr_learners].
#'
#' @section Usage:
#' ```
#' # Construction
#' l = Learner$new(id, task_type, feature_types= character(0L), predict_types = character(0L), packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L))
#' l = LearnerClassif$new(id, feature_types = character(0L), predict_types = "response", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L))
#' l = LearnerRegr$new(id, feature_types = character(0L), predict_types = "response", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L))
#' #
#' l$id
#' l$task_type
#' l$feature_types
#' l$predict_types
#' l$predict_type
#' l$packages
#' l$param_set
#' l$param_vals
#' l$properties
#' l$train(task)
#' l$predict(task, model)
#' l$hash
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):\cr
#'   Identifier for this object.
#' * `task_type` (`character(1)`):\cr
#'   Type of the task the learner can operator on. E.g., `"classif"` or `"regr"`.
#' * `feature_types` (`character()`):\cr
#'   Feature types the learner operates on. Must be a subset of `mlr_reflections$task_feature_types`.
#' * `predict_types` (`character()`):\cr
#'   Supported predict types. Must be a subset of `mlr_reflections$predict_types`.
#' * `packages` (`character()`]:\cr
#'   Set of required packages.
#' * `param_set` ([paradox::ParamSet]):\cr
#'   Set of required packages.
#' * `param_vals` (`named list()`):\cr
#'   List of hyperparameters.
#' * `properties` (`character()`):\cr
#'   Set of properties of the learner. Must be a subset of `mlr_reflections$learner_properties`.
#' * `task` ([Task]):\cr
#'   Task to train/predict on.
#' * `model`:\cr
#'   Arbitrary fitted model as returned by `train()`.
#'
#' @section Details:
#' * `$new()` creates a new object of class [Learner].
#' * `$id` (`character(1)`) stores the identifier of the object.
#' * `$task_type` (`character(1)`) stores the type of class this learner can operate on, e.g. `"classif"` or `"regr"`.
#' * `$feature_types` (`character()`) stores the feature types the learner can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
#' * `$predict_types` (`character()`) stores the possible predict types the learner is capable of. For classification,
#'   feasible values are `"response"` and `"prob"`, for regression `"response"` and `"se"` can be specified.
#' * `$predict_type` (`character(1)`) stores the currently selected predict type.
#' * `$packages` (`character()`) stores the names of required packages.
#' * `$param_set()` ([paradox::ParamSet]) describes the available hyperparameter and possible settings.
#' * `$param_vals()` (`named list()`) stores the list set hyperparameter values.
#' * `$properties` (`character()`) is a set of tags which describe the properties of the learner.
#' * `$train()` takes a task and returns a model fitted on all observations.
#' * `$predict()` takes a task and the model fitted in `$train()` to return predicted labels.
#' * `$hash` stores a checksum (`character(1)`) calculated on the `id` and `param_vals`.
#'
#' @name Learner
#' @family Learner
NULL

#' @export
Learner = R6Class("Learner",
  public = list(
    id = NULL,
    task_type = NULL,
    feature_types = NULL,
    predict_types = NULL,
    packages = NULL,
    properties = NULL,
    param_set = NULL,

    initialize = function(id, task_type, feature_types= character(0L), predict_types = character(0L), packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L)) {
      self$id = assert_id(id)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types)
      self$feature_types = assert_subset(feature_types, mlr_reflections$task_feature_types)
      self$predict_types = assert_subset(predict_types, mlr_reflections$predict_types[[task_type]], empty.ok = FALSE)
      self$packages = assert_set(packages)
      self$properties = assert_set(properties)
      self$param_set = assert_param_set(param_set)
      private$.param_vals = assert_param_vals(param_vals, param_set)
    },

    train = function(...) stopf("Method not implemented, should have been overloaded during construction"),
    predict = function(...) stopf("Method not implemented, should have been overloaded during construction"),
    print = function(...) {
      catf("Learner '%s' for %s", self$id, self$task_type)
      catf("Parameters: %s", as_short_string(self$param_vals, 1000L))
      catf("Feature types: %s", paste0(self$feature_types, collapse = ","))
    }
  ),

  active = list(
    hash = function() {
      if (is.na(private$.hash))
        private$.hash = digest::digest(list(self$id, self$param_vals), algo = "xxhash64")
      private$.hash
    },

    param_vals = function(rhs) {
      if (missing(rhs))
        return(private$.param_vals)
      private$.param_vals = assert_param_vals(rhs, self$param_set)
      private$.hash = NA_character_
    },

    predict_type = function(rhs) {
      if (missing(rhs))
        return(private$.predict_type)
      assert_choice(rhs, mlr_reflections$predict_types[[self$task_type]])
      if (rhs %nin% self$predict_types)
        stopf("Learner does not support predict type '%s'", rhs)
      private$.predict_type = rhs
    }
  ),
  private = list(
    .hash = NA_character_,
    .param_vals = NULL,
    .predict_type = NULL
  )
)
