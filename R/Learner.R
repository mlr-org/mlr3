#' @title Learner Class
#'
#' @name Learner
#' @format [R6Class] object.
#' @description
#' Predefined learners are stored in [mlr_learners].
#'
#' @section Usage:
#' ```
#' # Construction
#' l = Learner$new(id, task_type, feature_types= character(0L), predict_types = character(0L), packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L))
#' l = LearnerClassif$new(id, feature_types = character(0L), predict_types = "response", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L))
#' l = LearnerRegr$new(id, feature_types = character(0L), predict_types = "response", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L))
#'
#' # Members
#' l$fallback
#' l$feature_types
#' l$hash
#' l$id
#' l$model
#' l$packages
#' l$param_set
#' l$param_vals
#' l$params_predict
#' l$params_train
#' l$predict_type
#' l$predict_types
#' l$properties
#' l$task_type
#'
#' # Methods
#' l$train(task)
#' l$predict(task)
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`): Identifier for this object.
#' * `task_type` (`character(1)`): Type of the task the learner can operator on. E.g., `"classif"` or `"regr"`.
#' * `feature_types` (`character()`): Feature types the learner operates on. Must be a subset of `mlr_reflections$task_feature_types`.
#' * `predict_types` (`character()`): Supported predict types. Must be a subset of `mlr_reflections$predict_types`.
#' * `packages` (`character()`]: Set of required packages.
#' * `param_set` ([paradox::ParamSet]): Set of hyperparameters.
#' * `param_vals` (named `list()`): List of hyperparameter settings.
#' * `properties` (`character()`): Set of properties of the learner. Must be a subset of `mlr_reflections$learner_properties`.
#' * `task` ([Task]): Task to train/predict on.
#'
#' @section Details:
#' * `$fallback` ([Learner] | `NULL`) optionally stores a fallback learner which
#'   is used to generate predictions if this learner fails to train or predict.
#'   This mechanism is disabled unless you explicitly assign a learner to this slot.
#' * `$feature_types` (`character()`) stores the feature types the learner can
#'   handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
#' * `$hash` (`character(1)`) stores a checksum calculated on the `id` and `param_vals`.
#'   This hash is cached internally.
#' * `$id` (`character(1)`) stores the identifier of the object.
#' * `$packages` (`character()`) stores the names of required packages.
#' * `$param_set` ([paradox::ParamSet]) describes the available hyperparameter
#'   and possible settings.
#' * `$param_vals` (named `list()`) stores the list set hyperparameter values.
#' * `$params_predict` (`list()`) stores the settings that have been used for prediction.
#' * `$params_train` (`list()`) stores the settings that have been used for training
#' * `$predict_type` (`character(1)`) stores the currently selected predict type.
#' * `$predict_types` (`character()`) stores the possible predict types the learner
#'   is capable of. For classification, feasible values are `"response"` and
#'   `"prob"`, for regression `"response"` and `"se"` can be specified.
#' * `$properties` (`character()`) is a set of tags which describe the properties
#'   of the learner.
#' * `$task_type` (`character(1)`) stores the type of class this learner can
#'   operate on, e.g. `"classif"` or `"regr"`.
#' * `$new()` creates a new object of class [Learner].
#' * `$predict()` takes a [Task] and uses `self$model` (fitted during train())
#'   to return a [Prediction] object.
#' * `$train()` takes a [Task], sets the slot `model` and returns `self`.
#'
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
    model = NULL,
    fallback = NULL,

    initialize = function(id, task_type, feature_types= character(0L), predict_types = character(0L), packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L)) {
      self$id = assert_id(id)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types)
      self$feature_types = assert_subset(feature_types, mlr_reflections$task_feature_types)
      self$predict_types = assert_subset(predict_types, mlr_reflections$predict_types[[task_type]], empty.ok = FALSE)
      self$packages = assert_set(packages)
      self$properties = sort(assert_set(properties))
      self$param_set = assert_param_set(param_set)
      private$.param_vals = assert_param_vals(param_vals, param_set)
    },

    train = function(...) stopf("Method not implemented, should have been overloaded during construction"),
    predict = function(...) stopf("Method not implemented, should have been overloaded during construction"),

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function() {
      learner_print(self)
    }
  ),

  active = list(
    hash = function() {
      if (is.na(private$.hash))
        private$.hash = hash(list(self$id, self$param_vals))
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
    },

    params_train = function() {
      # TODO: move some logic to paradox
      pv = self$param_vals
      pv[map_lgl(self$param_set$tags[names(pv)], is.element, el = "train")]
    },

    params_predict = function() {
      # TODO: move some logic to paradox
      pv = self$param_vals
      pv[map_lgl(self$param_set$tags[names(pv)], is.element, el = "predict")]
    }
  ),

  private = list(
    .hash = NA_character_,
    .param_vals = NULL,
    .predict_type = NULL
  )
)

learner_print = function(self) {
  catf(format(self))
  catf(str_indent("Parameters:", as_short_string(self$param_vals, 1000L)))
  catf(str_indent("Packages:", self$packages))
  catf(str_indent("Predict Type:", self$predict_type))
  catf(str_indent("Feature types:", self$feature_types))
  catf(str_indent("Properties:", self$properties))
  if (!is.null(self$fallback))
    catf(str_indent("Fallback:", format(self$fallback)))
  catf(str_indent("\nPublic:", str_r6_interface(self)))
}
