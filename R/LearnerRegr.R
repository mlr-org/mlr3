#' @title Regression Learner
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Learner].
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] for regression problems.
#' The slot `task_type` is set to `"regr"`.
#' Predefined learners can be found in the [Dictionary] [mlr_learners].
#'
#' @section Construction:
#' ```
#' l = LearnerRegr$new(id, feature_types = character(0L), predict_types = character(0L),
#'   packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L))
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier for the learner.
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
#' @inheritSection Learner Public
#' @inheritSection Learner Methods
#'
#' @family Learner
#' @export
#' @examples
#' # get all regression learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^regr"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' lrn = mlr_learners$get("regr.rpart")
#' print(lrn)
LearnerRegr = R6Class("LearnerRegr", inherit = Learner,
  public = list(
    initialize = function(id, feature_types = character(0L), predict_types = "response", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L)) {
      super$initialize(id = id, task_type = "regr", feature_types = feature_types, predict_types = predict_types,
        packages = packages, param_set = param_set, param_vals = param_vals, properties = properties)
    }
  )
)
