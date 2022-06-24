#' @title Classification Learner
#'
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] for classification problems:
#'
#' * `task_type` is set to `"classif"`.
#' * Creates [Prediction]s of class [PredictionClassif].
#' * Possible values for `predict_types` are:
#'   - `"response"`: Predicts a class label for each observation in the test set.
#'   - `"prob"`: Predicts the posterior probability for each class for each observation in the test set.
#' * Additional learner properties include:
#'   - `"twoclass"`: The learner works on binary classification problems.
#'   - `"multiclass"`: The learner works on multiclass classification problems.
#'
#' Predefined learners can be found in the [dictionary][mlr3misc::Dictionary] [mlr_learners].
#' Essential classification learners can be found in this dictionary after loading \CRANpkg{mlr3learners}.
#' Additional learners are implement in the Github package \url{https://github.com/mlr-org/mlr3extralearners}.
#'
#' @template param_id
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_data_formats
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @template seealso_learner
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
#' task = tsk("penguins")
#' lrn$train(task, 1:200)
#'
#' # predict on new observations:
#' lrn$predict(task, 201:344)$confusion
LearnerClassif = R6Class("LearnerClassif", inherit = Learner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character(), label = NA_character_, man = NA_character_) {
      super$initialize(id = id, task_type = "classif", param_set = param_set, predict_types = predict_types,
        feature_types = feature_types, properties = properties, data_formats = data_formats, packages = packages,
        label = label, man = man)
    }
  )
)
