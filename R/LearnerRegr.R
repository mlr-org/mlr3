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
    initialize = function(id, param_set = ParamSet$new(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character(), man = NA_character_) {
      super$initialize(id = id, task_type = "regr", param_set = param_set, feature_types = feature_types,
        predict_types = predict_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    }
  )
)
