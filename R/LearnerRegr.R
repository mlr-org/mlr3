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
#' l = LearnerRegr$new(id, param_set = ParamSet$new(), param_vals = list(), predict_types = character(),
#'      feature_types = character(), properties = character(), data_formats = "data.table", packages = character())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the learner.
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of hyperparameters.
#'
#' * `param_vals` :: named `list()`\cr
#'   List of hyperparameter settings.
#'
#' * `predict_types` :: `character()`\cr
#'   Supported predict types. Must be a subset of [`mlr_reflections$learner_predict_types`][mlr_reflections].
#'
#' * `feature_types` :: `character()`\cr
#'   Feature types the learner operates on. Must be a subset of `mlr_reflections$task_feature_types`.
#'
#' * `properties` :: `character()`\cr
#'   Set of properties of the learner. Must be a subset of [`mlr_reflections$learner_properties`][mlr_reflections].
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'
#' @section Fields:
#' @inheritSection Learner Fields
#'
#' @section Methods:
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
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character()) {
      super$initialize(id = id, task_type = "regr", param_set = param_set, param_vals = param_vals,
        feature_types = feature_types, predict_types = predict_types, properties = properties,
        data_formats = data_formats, packages = packages)
    }
  )
)
