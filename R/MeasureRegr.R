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
#' @template param_range
#' @template param_minimize
#' @template param_average
#' @template param_aggregator
#' @template param_predict_type
#' @template param_measure_properties
#' @template param_predict_sets
#' @template param_task_properties
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
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, range, minimize = NA, average = "macro", aggregator = NULL, properties = character(), predict_type = "response",
      predict_sets = "test", task_properties = character(), packages = character(), man = NA_character_) {
      super$initialize(id, task_type = "regr", range = range, minimize = minimize, average = average, aggregator = aggregator,
        properties = properties, predict_type = predict_type, predict_sets = predict_sets,
        task_properties = task_properties, packages = packages, man = man)
    }
  )
)
