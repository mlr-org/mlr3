#' @title Classification Learner
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Learner].
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] for classification problems.
#'
#' Many predefined learners can be found in the [mlr3misc::Dictionary] [mlr_learners] after loading the \CRANpkg{mlr3learners} package.
#'
#' @section Construction:
#' ```
#' l = LearnerClassif$new(id, param_set = ParamSet$new(), param_vals = list(), predict_types = character(),
#'      feature_types = character(), properties = character(), data_formats = "data.table", packages = character())
#' ```
#' For a description of the arguments, see [Learner].
#' `task_type` is set to `"classif"`.
#' Possible values for `predict_types` are a subset of `c("response", "prob")`.
#'
#' @section Fields:
#' See [Learner].
#'
#' @section Methods:
#' See [Learner].
#'
#' @family Learner
#' @seealso
#' Example classification learners: [`classif.rpart`][mlr_learners_classif.rpart]
#' @export
#' @examples
#' # get all classification learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^classif"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' lrn = lrn("classif.rpart")
#' print(lrn)
#'
#' # train the learner:
#' task = tsk("iris")
#' lrn$train(task, 1:120)
#'
#' # predict on new observations:
#' lrn$predict(task, 121:150)$confusion
LearnerClassif = R6Class("LearnerClassif", inherit = Learner,
  public = list(
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character()) {
      super$initialize(id = id, task_type = "classif", param_set = param_set, param_vals = param_vals,
        predict_types = predict_types, feature_types = feature_types, properties = properties,
        data_formats = data_formats, packages = packages)
    }
  )
)
