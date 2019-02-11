#' @title Learner Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the abstract base class for learner objects like [LearnerClassif] and [LearnerRegr].
#'
#' @section Construction:
#' ```
#' l = Learner$new(id, task_type, feature_types = character(0L), predict_types = character(0L),
#'   packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L))
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier for the learner.
#' * `task_type` :: `character(1)`\cr
#'   Type of the task the learner can operator on. E.g., `"classif"` or `"regr"`.
#' * `feature_types` :: `character()`\cr
#'   Feature types the learner operates on. Must be a subset of `mlr_reflections$task_feature_types`.
#' * `predict_types` :: `character()`\cr
#'   Supported predict types. Must be a subset of [`mlr_reflections$predict_types`][mlr_reflections].
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of hyperparameters.
#' * `param_vals` :: named `list()`\cr
#'   List of hyperparameter settings.
#' * `properties` :: `character()`\cr
#'   Set of properties of the learner. Must be a subset of [`mlr_reflections$learner_properties`][mlr_reflections].
#'
#' @section Public:
#' * `fallback` :: ([Learner] | `NULL`)\cr
#'   Optionally stores a second [Learner] which is activated as fallback if this first [Learner] fails during
#'   train or predict.
#'   This mechanism is disabled unless you explicitly assign a learner to this slot.
#'   Additionally, you need to catch raised exceptions via encapsulation, see [mlr_control()].
#'
#' * `feature_types` :: `character()`\cr
#'   Stores the feature types the learner can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
#'   A complete list of candidate feature types, grouped by task type, is stored in [`mlr_reflections$task_feature_types`][mlr_reflections].
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) of the Learner.
#'
#' * `id` :: `character(1)`\cr
#'   Stores the identifier of the object.
#'
#' * `packages` :: `character()`\cr
#'   Stores the names of required packages. Note that these packages will be loaded via [requireNamespace()],
#'   and are not attached.
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Description of available hyperparameters and hyperparameter settings.
#'
#' * `predict_type` :: `character(1)`\cr
#'   Stores the currently selected predict type. Must be an element of `l$predict_types`.
#'
#' * `predict_types` :: `character()`\cr
#'   Stores the possible predict types the learner is capable of.
#'   A complete list of candidate predict types, grouped by task type, is stored in [`mlr_reflections$predict_types`][mlr_reflections].
#'
#' * `properties` :: `character()`\cr
#'   Stores a set of properties/capabilities the learner has.
#'   A complete list of candidate properties, grouped by task type, is stored in [`mlr_reflections$learner_properties`][mlr_reflections].
#'
#' * `task_type` :: `character(1)`\cr
#'   Stores the type of class this learner can operate on, e.g. `"classif"` or `"regr"`.
#'   A complete list of task types is stored in `mlr_reflections$task_types`.
#'
#'
#'
#' @section Methods:
#' * `params(tag)`\cr
#'   `character(1)` -> named `list()`\cr
#'   Returns a list of hyperparameter settings from `param_set` where the corresponding parameters in `param_set` are tagged
#'   with `tag`. I.e., `l$params("train")` returns all settings of hyperparameters used in the training step.
#'
#' * `train(task)`\cr
#'   [Task] -> `self`\cr
#'   Train the learner on the complete [Task]. The resulting model is stored in `l$model`.
#'
#' * `predict(task)`\cr
#'   [Task] -> [Prediction]\cr
#'   Uses `l$model` (fitted during `train()`) to return a [Prediction] object.
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
#' @export
Learner = R6Class("Learner",
  public = list(
    task_type = NULL,
    feature_types = NULL,
    predict_types = NULL,
    packages = NULL,
    properties = NULL,
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
      private$.param_set = assert_param_set(param_set)
      private$.param_set$values = param_vals
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
    },
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .calculate_hash = function() {
      hash(list(class(self), private$.id, self$param_set$values, private$.predict_type))
    },
    .predict_type = NULL,
    .param_set = NULL
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
