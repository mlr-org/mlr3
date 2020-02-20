#' @title Regression Measure
#'
#' @include Measure.R
#'
#' @description
#' This measure specializes [Measure] for regression problems:
#'
#' * `task_type` is set to `"regr"`.
#' * Possible values for `predict_type` are `"response"` and `"se"`.
#'
#' Predefined measures can be found in the [mlr3misc::Dictionary] [mlr_measures].
#'
#' @template param_id
#' @template param_packages
#' @template param_man
#'
#' @family Measure
#' @seealso
#' Default regression measures: [`regr.mse`][mlr_measures_regr.mse]
#' @export
MeasureRegr = R6Class("MeasureRegr", inherit = Measure, cloneable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of the [R6][R6::R6Class] object.
    #'
    #' @param range (`numeric(2)`)\cr
    #'   Feasible range for this measure as `c(lower_bound, upper_bound)`.
    #'   Both bounds may be infinite.
    #'
    #' @param minimize (`logical(1)`)\cr
    #'   Set to `TRUE` if good predictions correspond to small values,
    #'   and to `FALSE` if good predictions correspond to large values.
    #'   If set to `NA` (default), tuning this measure is not possible.
    #'
    #' @param average (`character(1)`)\cr
    #'   How to average multiple [Prediction]s from a [ResampleResult].
    #'
    #'   The default, `"macro"`, calculates the individual performances scores for each [Prediction] and then uses the
    #'   function defined in `aggregator` to average them to a single number.
    #'
    #'   If set to `"micro"`, the individual [Prediction] objects are first combined into a single new [Prediction] object which is then used to assess the performance.
    #'   The function `aggregator` is not used in this case.
    #'
    #' @param aggregator (`function(x)`)\cr
    #'   Function to aggregate individual performance scores `x` where `x` is a numeric vector.
    #'   If `NULL`, defaults to [mean()].
    #'
    #' @param properties (`character()`)\cr
    #'   Properties of the measure.
    #'   Must be a subset of [mlr_reflections$measure_properties][mlr_reflections].
    #'   Supported by `mlr3`:
    #'   * `"requires_task"` (requires the complete [Task]),
    #'   * `"requires_learner"` (requires the trained [Learner]),
    #'   * `"requires_train_set"` (requires the training indices from the [Resampling]), and
    #'   * `"na_score"` (the measure is expected to occasionally return `NA`).
    #'
    #' @param predict_type (`character(1)`)\cr
    #'   Required predict type of the [Learner].
    #'   Possible values are stored in [mlr_reflections$learner_predict_types][mlr_reflections].
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Prediction sets to operate on, used in `aggregate()` to extract the matching `predict_sets` from the [ResampleResult].
    #'   Multiple predict sets are calculated by the respective [Learner] during [resample()]/[benchmark()].
    #'   Must be a non-empty subset of `c("train", "test")`.
    #'   If multiple sets are provided, these are first combined to a single prediction object.
    #'   Default is `"test"`.
    #'
    #' @param task_properties (`character()`)\cr
    #'   Required task properties, see [Task].
    initialize = function(id, range, minimize = NA, average = "macro", aggregator = NULL, properties = character(), predict_type = "response",
      predict_sets = "test", task_properties = character(), packages = character(), man = NA_character_) {
      super$initialize(id, task_type = "regr", range = range, minimize = minimize, average = average, aggregator = aggregator,
        properties = properties, predict_type = predict_type, predict_sets = predict_sets,
        task_properties = task_properties, packages = packages, man = man)
    }
  )
)
