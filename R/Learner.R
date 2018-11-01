#' @title Learner Class
#'
#' @description
#' Predefined learners are stored in [mlr_learners].
#'
#' @section Usage:
#' ```
#' # Construction
#' l = Learner$new(id, task_type, feature_types= character(0L), predict_types = character(0L), packages = character(0L), par_set = ParamSet$new(), par_vals = list(), properties = character(0L))
#' l = LearnerClassif$new(id, feature_types = character(0L), predict_types = "response", packages = character(0L), par_set = ParamSet$new(), par_vals = list(), properties = character(0L))
#' l = LearnerRegr$new(id, feature_types = character(0L), predict_types = "response", packages = character(0L), par_set = ParamSet$new(), par_vals = list(), properties = character(0L))
#' #
#' l$id
#' l$task_type
#' l$feature_types
#' l$predict_types
#' l$predict_type
#' l$packages
#' l$par_set
#' l$par_vals
#' l$properties
#' l$train(task)
#' l$predict(task, model)
#' l$hash
#' ```
#'
#' @section Arguments:
#' * `id` \[`character(1)`\]:
#'   identifier for this object.
#' * `task_type` \[`character(1)`\]:\cr
#'   Type of the task the learner can operator on. E.g., `"classif"` or `"regr"`.
#' * `feature_types` \[`character()`\]:\cr
#'   Feature types the learner operates on. Must be a subset of `mlr_reflections$task_feature_types`.
#' * `predict_types` \[`character()`\]:\cr
#'   Supported predict types. Must be a subset of `mlr_reflections$predict_types`.
#' * `packages` \[`character()`]:\cr
#'   Set of required packages.
#' * `par_set` \[[`ParamSet`][paradox::ParamSet]\]:\cr
#'   Set of required packages.
#' * `par_vals` \[`named list()`\]:\cr
#'   List of hyperparameters.
#' * `properties` \[`character()`\]:\cr
#'   Set of properties of the learner. Must be a subset of `mlr_reflections$learner_properties`.
#' * `task` \[[Task]\]:\cr
#'   Task to train/predict on.
#' * `model` \[any\]:\cr
#'   Fitted model as returned by `train`.
#'
#' @section Details:
#' `$new()` creates a new object of class [Learner].
#'
#' `$id` \[`character(1)`\] stores the identifier of the object.
#'
#' `$task_type` \[`character(1)`\] stores the type of class this learner can operate on, e.g. `"classif"` or `"regr"`.
#'
#' `$feature_types` \[`character()`\] stores the feature types the learner can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
#'
#' `$predict_types` \[`character()`\] stores the possible predict types the learner is capable of. For classification,
#'   feasible values are `"response"` and `"prob"`, for regression `"response"` and `"se"` can be specified.
#'
#' `$predict_type` \[`character(1)`\] stores the currently selected predict type.
#'
#' `$packages` \[`character()`\] stores the names of required packages.
#'
#' `$par_set()` \[[paradox::ParamSet]\] describes the available hyperparameter and possible settings.
#'
#' `$par_vals()` \[`named list()`\] stores the list set hyperparameter values.
#'
#' `$properties` \[`character()`\] is a set of tags which describe the properties of the learner.
#'
#' `$train()` takes a task and returns a model fitted on all observations.
#'
#' `$predict()` takes a task and the model fitted in `$train()` to return predicted labels.
#'
#' `$hash` stores a checksum (`character(1)`) calculated on the `id` and `par_vals`.
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
    par_set = NULL,

    initialize = function(id, task_type, feature_types= character(0L), predict_types = character(0L), packages = character(0L), par_set = ParamSet$new(), par_vals = list(), properties = character(0L)) {
      self$id = assert_id(id)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types)
      self$feature_types = assert_subset(feature_types, mlr_reflections$task_feature_types)
      self$predict_types = assert_subset(predict_types, mlr_reflections$predict_types[[task_type]], empty.ok = FALSE)
      self$packages = assert_set(packages)
      self$properties = assert_set(properties)
      self$par_set = assert_par_set(par_set)
      private$.par_vals = assert_par_vals(par_vals, par_set)
    },

    train = function(...) stopf("Method not implemented, should have been overloaded during construction"),
    predict = function(...) stopf("Method not implemented, should have been overloaded during construction"),
    print = function(...) {
      catf("Learner '%s' for %s", self$id, self$task_type)
      catf("Parameters: %s", stri_key_val(self$par_vals))
      catf("Feature types: %s", stri_head(self$feature_types, 10L, quote = ""))
    }
  ),

  active = list(
    hash = function() {
      if (is.na(private$.hash))
        private$.hash = digest::digest(list(self$id, self$par_vals), algo = "xxhash64")
      private$.hash
    },

    par_vals = function(rhs) {
      if (missing(rhs))
        return(private$.par_vals)
      private$.par_vals = assert_par_vals(rhs, self$par_set)
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
    .par_vals = NULL,
    .predict_type = NULL
  )
)
