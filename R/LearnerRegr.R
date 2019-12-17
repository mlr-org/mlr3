#' Regression Learner
#'
#' @description
#' This Learner specializes [Learner] for regression problems.
#' Many predefined learners can be found in the [mlr3misc::Dictionary] [mlr_learners] after loading the \CRANpkg{mlr3learners} package.
#'
#' @examples
#' # get all regression learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^regr"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' mlr_learners$get("regr.rpart")
#' lrn("classif.featureless")
#'
#' @seealso
#' Example regression learners: [`regr.rpart`][mlr_learners_regr.rpart]
#'
#' @export
LearnerRegr = R6Class("LearnerRegr", inherit = Learner,
  public = list(
    #' @description
    #' For a description of the arguments, see [Learner].
    #' `task_type` is set to `"regr"`.
    #'
    #' @param id Identifier of the learner.
    #' @param param_set Description of available hyperparameters and hyperparameter settings.
    #' @param predict_types Stores the possible predict types the learner is capable of.
    #'   A complete list of candidate predict types, grouped by task type, is stored in [`mlr_reflections$learner_predict_types`][mlr_reflections].
    #' @param feature_types Stores the feature types the learner can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
    #'   A complete list of candidate feature types, grouped by task type, is stored in [`mlr_reflections$task_feature_types`][mlr_reflections].
    #' @param properties Stores a set of properties/capabilities the learner has.
    #'   A complete list of candidate properties, grouped by task type, is stored in [`mlr_reflections$learner_properties`][mlr_reflections].
    #' @param data_formats Vector of supported data formats which can be processed during `$train()` and `$predict()`.
    #'   Defaults to `"data.table"`.
    #' @param packages Stores the names of required packages.
    #' @param man String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    initialize = function(id, param_set = ParamSet$new(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character(), man = NA_character_) {
      super$initialize(id = id, task_type = "regr", param_set = param_set, feature_types = feature_types,
        predict_types = predict_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    }
  )
)
