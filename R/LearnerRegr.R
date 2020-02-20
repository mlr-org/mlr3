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
    initialize = function(id, param_set = ParamSet$new(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character(), man = NA_character_) {
      super$initialize(id = id, task_type = "regr", param_set = param_set, feature_types = feature_types,
        predict_types = predict_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    }
  )
)
