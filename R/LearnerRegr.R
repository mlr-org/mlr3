#' @title Regression Learner
#'
#' @name LearnerRegr
#' @format [R6Class] object inheriting from [Learner].
#' @description
#' This Learner specializes [Learner] for regression problems.
#'
#' @section Usage:
#' See [Learner].
#'
#' @section Details:
#' `$task_type` is `"regr"`.
#'
#' @family Learner
#' @examples
#' # get all regression learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$ids("^regr"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' lrn = mlr_learners$get("regr.rpart")
#' print(lrn)
NULL

#' @export
LearnerRegr = R6Class("LearnerRegr", inherit = Learner,
  public = list(
    initialize = function(id, feature_types = character(0L), predict_types = "response", packages = character(0L), param_set = ParamSet$new(), param_vals = list(), properties = character(0L)) {
      super$initialize(id = id, task_type = "regr", feature_types = feature_types, predict_types = predict_types,
        packages = packages, param_set = param_set, param_vals = param_vals, properties = properties)
    }
  )
)
