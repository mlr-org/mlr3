#' Classification Learner
#'
#' @description
#' This Learner specializes [Learner] for classification problems.
#' Many predefined learners can be found in the [mlr3misc::Dictionary] [mlr_learners] after loading the \CRANpkg{mlr3learners} package.
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
#'
#' @seealso
#' Example classification learners: [LearnerClassifRpart]
#'
#' @export
LearnerClassif = R6Class("LearnerClassif", inherit = Learner,
  public = list(
    #' @description
    #' For a description of the arguments, see [Learner].
    #' `task_type` is set to `"classif"`.
    #'
    #' Additional learner properties include:
    #' * `"twoclass"`: The learner works on binary classification problems.
    #' * `"multiclass"`: The learner works on multiclass classification problems.
    #'
    #' @param id Identifier for the learner.
    #' @param param_Set Set of hyperparameters.
    #' @param predict_types Supported predict types. Must be a subset of [`mlr_reflections$learner_predict_types`][mlr_reflections].
    #'   Possible values for `predict_types` are passed to and converted by [PredictionClassif]:
    #'   * `"response"`: Predicts a class label for each observation in the test set.
    #'   * `"prob"`: Predicts the posterior probability for each class for each observation in the test set.
    #' @param feature_types Feature types the learner operates on. Must be a subset of [`mlr_reflections$task_feature_types`][mlr_reflections].
    #' @param properties Set of properties of the learner. Must be a subset of [`mlr_reflections$learner_properties`][mlr_reflections].
    #'   The following properties are currently standardized and understood by learners in \CRANpkg{mlr3}:
    #'   * `"missings"`: The learner can handle missing values in the data.
    #'   * `"weights"`: The learner supports observation weights.
    #'   * `"importance"`: The learner supports extraction of importance scores, i.e. comes with a `importance()` extractor function (see section on optional extractors).
    #'   * `"selected_features"`: The learner supports extraction of the set of selected features, i.e. comes with a `selected_features()` extractor function (see section on optional extractors).
    #'   * `"oob_error"`: The learner supports extraction of estimated out of bag error, i.e. comes with a `oob_error()` extractor function (see section on optional extractors).
    #' @param data_formats Vector of supported data formats which can be processed during `$train()` and `$predict()`. Defaults to `"data.table"`.
    #' @param packages Set of required packages. Note that these packages will be loaded via [requireNamespace()], and are not attached.
    #' @param man String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    #' @return An object representing the learner.
    initialize = function(id, param_set = ParamSet$new(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character(), man = NA_character_) {
      super$initialize(id = id, task_type = "classif", param_set = param_set, predict_types = predict_types,
        feature_types = feature_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    }
  )
)
