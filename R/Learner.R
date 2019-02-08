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
#' l$predict_type
#' l$predict_types
#' l$properties
#' l$task_type
#'
#' # Methods
#' l$train(task)
#' l$predict(task)
#' l$params(tag)
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
#' * `tag` (`character(1)`): Tag of parameters.
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
#' * `$params()` returns a list of hyperparameter settings from `param_vals` where the corresponding parameters in `param_set` are tagged
#'    with `tag`. I.e., `l$params("train")` returns all settings of hyperparameters used in the training step.
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
#'
#' @section Optional Extractors:
#'
#' Specific learner implementations are free to implement additional getters to ease the access of certain parts
#' of the model in the inherited subclasses.
#'
#' For the following operations, extractors are standardized:
#'
#' * `importance(...)`: Returns a feature importance score as `numeric()`.
#'   The learner must be tagged with property "importance".
#'
#'   The higher the score, the more important the variable.
#'   The returned vector is named with feature names and sorted in decreasing order.
#'   Note that the model might omit features it has not used at all.
#'
#' * `selected_features(...)`: Returns a subset of selected features as `character()`.
#'   The learner must be tagged with property "selected_features".
#'
#' @family Learner
NULL

#' @export
Learner = R6Class("Learner",
  public = list(
    task_type = NULL,
    feature_types = NULL,
    predict_types = NULL,
    packages = NULL,
    properties = NULL,
    param_set = NULL,
    model = NULL,
    fallback = NULL,

    initialize = function(id, task_type, feature_types = character(0L), predict_types = character(0L), packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L)) {
      private$.id = id
      self$task_type = assert_choice(task_type, mlr_reflections$task_types)
      self$feature_types = assert_sorted_subset(feature_types, mlr_reflections$task_feature_types)
      self$predict_types = assert_sorted_subset(predict_types, mlr_reflections$predict_types[[task_type]], empty.ok = FALSE)
      private$.predict_type = predict_types[1L]
      self$packages = assert_set(packages)
      self$properties = sort(assert_subset(properties, mlr_reflections$learner_properties[[task_type]]))
      self$param_set = assert_param_set(param_set)
      self$param_set$values = param_vals
    },

    train = function(...) stopf("Method not implemented, should have been overloaded during construction"),
    predict = function(...) stopf("Method not implemented, should have been overloaded during construction"),

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function() {
      learner_print(self)
    },

    params = function(tag) {
      assert_string(tag)
      pv = self$param_set$values
      pv[map_lgl(self$param_set$tags[names(pv)], is.element, el = tag)]
    }
  ),

  active = list(
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
    .calculate_hash = function() {
      hash(list(class(self), private$.id, self$param_set$values, private$.predict_type))
    },
    .predict_type = NULL
  )
)

Learner = add_id_hash(Learner)

learner_print = function(self) {
  catf(format(self))
  catf(str_indent("Parameters:", as_short_string(self$param_set$values, 1000L)))
  catf(str_indent("Packages:", self$packages))
  catf(str_indent("Predict Type:", self$predict_type))
  catf(str_indent("Feature types:", self$feature_types))
  catf(str_indent("Properties:", self$properties))
  if (!is.null(self$fallback))
    catf(str_indent("Fallback:", format(self$fallback)))
  catf(str_indent("\nPublic:", str_r6_interface(self)))
}
