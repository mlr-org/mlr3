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
#' Predefined learners can be found in the [mlr3misc::Dictionary] [mlr_learners].
#' Essential classification learners can be found in this dictionary after loading \CRANpkg{mlr3learners}.
#'
#' @family Learner
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
    #' @description
    #' Creates a new instance of the [R6][R6::R6Class] object.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the learner.
    #'
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Set of hyperparameters.
    #'
    #' @param predict_types (`character()`)\cr
    #'   Supported predict types. Must be a subset of [`mlr_reflections$learner_predict_types`][mlr_reflections].
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Sets to predict on during [resample()]/[benchmark()].
    #'   Creates and stores a separate [Prediction] object for each set.
    #'   The individual sets can be combined via getters in [ResampleResult]/[BenchmarkResult], or [Measure]s can be set to operate on subsets of the calculated prediction sets.
    #'   Must be a non-empty subset of `("train", "test")`.
    #'   Default is `"test"`.
    #'
    #' @param feature_types (`character()`)\cr
    #'   Feature types the learner operates on. Must be a subset of [`mlr_reflections$task_feature_types`][mlr_reflections].
    #'
    #' @param properties (`character()`)\cr
    #'   Set of properties of the learner. Must be a subset of [`mlr_reflections$learner_properties`][mlr_reflections].
    #'   The following properties are currently standardized and understood by learners in \CRANpkg{mlr3}:
    #'   * `"missings"`: The learner can handle missing values in the data.
    #'   * `"weights"`: The learner supports observation weights.
    #'   * `"importance"`: The learner supports extraction of importance scores, i.e. comes with a `importance()` extractor function (see section on optional extractors).
    #'   * `"selected_features"`: The learner supports extraction of the set of selected features, i.e. comes with a `selected_features()` extractor function (see section on optional extractors).
    #'   * `"oob_error"`: The learner supports extraction of estimated out of bag error, i.e. comes with a `oob_error()` extractor function (see section on optional extractors).
    #'
    #' @param data_formats (`character()`)\cr
    #'   Vector of supported data formats which can be processed during `$train()` and `$predict()`.
    #'   Defaults to `"data.table"`.
    #'
    #' @param packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled by the constructor if at least one of the packages is not installed.
    #'   The packages will be loaded (not attached) via [requireNamespace()] for `$train()`/`$predict()`.
    #'
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    initialize = function(id, param_set = ParamSet$new(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character(), man = NA_character_) {
      super$initialize(id = id, task_type = "classif", param_set = param_set, predict_types = predict_types,
        feature_types = feature_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    }
  )
)
