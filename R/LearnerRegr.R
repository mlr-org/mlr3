#' @title Regression Learner
#'
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] for regression problems:
#'
#' * `task_type` is set to `"regr"`.
#' * Creates [Prediction]s of class [PredictionRegr].
#' * Possible values for `predict_types` are:
#'   - `"response"`: Predicts a numeric response for each observation in the test set.
#'   - `"se"`: Predicts the standard error for each value of response for each observation in the test set.
#'
#' Predefined learners can be found in the [mlr3misc::Dictionary] [mlr_learners].
#' Essential regression learners can be found in this dictionary after loading \CRANpkg{mlr3learners}.
#'
#' @template param_id
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_data_formats
#' @template param_packages
#' @template param_man
#'
#' @family Learner
#' @export
#' @examples
#' # get all regression learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^regr"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' mlr_learners$get("regr.rpart")
#' lrn("classif.featureless")
LearnerRegr = R6Class("LearnerRegr", inherit = Learner,
  public = list(
    #' @description
    #' Creates a new instance of the [R6][R6::R6Class] object.
    initialize = function(id, param_set = ParamSet$new(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character(), man = NA_character_) {
      super$initialize(id = id, task_type = "regr", param_set = param_set, feature_types = feature_types,
        predict_types = predict_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    }
  )
)
