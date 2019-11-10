#' Classification Learner
#'
#' This Learner specializes [Learner] for classification problems.
#' Many predefined learners can be found in the [mlr3misc::Dictionary] [mlr_learners] after loading the \CRANpkg{mlr3learners} package.
#'
#' @section Construction:
#' ```
#' l = LearnerClassif$new(id, param_set = ParamSet$new(), predict_types = character(), feature_types = character(),
#'     properties = character(), data_formats = "data.table", packages = character(), man = NA_character_)
#' ```
#' For a description of the arguments, see [Learner].
#' `task_type` is set to `"classif"`.
#'
#' Possible values for `predict_types` are passed to and converted by [PredictionClassif]:
#'
#' * `"response"`: Predicts a class label for each observation in the test set.
#' * `"prob"`: Predicts the posterior probability for each class for each observation in the test set.
#'
#' Additional learner properties include:
#' * `"twoclass"`: The learner works on binary classification problems.
#' * `"multiclass"`: The learner works on multiclass classification problems.
#'
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
#' @export
LearnerClassif = R6Class("LearnerClassif", inherit = Learner,
  public = list(
    initialize = function(id, param_set = ParamSet$new(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character(), man = NA_character_) {
      super$initialize(id = id, task_type = "classif", param_set = param_set, predict_types = predict_types,
        feature_types = feature_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    }
  )
)
