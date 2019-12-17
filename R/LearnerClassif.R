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
      super$initialize(id = id, task_type = "classif", param_set = param_set, predict_types = predict_types,
        feature_types = feature_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    }
  )
)
