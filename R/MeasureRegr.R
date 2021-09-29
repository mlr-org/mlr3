#' @title Regression Measure
#'
#' @include Measure.R
#'
#' @description
#' This measure specializes [Measure] for regression problems:
#'
#' * `task_type` is set to `"regr"`.
#' * Possible values for `predict_type` are `"response"`, `"se"` and `"distr"`.
#'
#' Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#' The default measure for regression is [`regr.mse`][mlr_measures_regr.mse].
#'
#' @template param_id
#' @template param_param_set
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
#' @template seealso_measure
#' @export
MeasureRegr = R6Class("MeasureRegr", inherit = Measure, cloneable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), range, minimize = NA, average = "macro", aggregator = NULL, properties = character(), predict_type = "response",
      predict_sets = "test", task_properties = character(), packages = character(), man = NA_character_) {
      super$initialize(id, task_type = "regr", param_set = param_set, range = range, minimize = minimize, average = average, aggregator = aggregator,
        properties = properties, predict_type = predict_type, predict_sets = predict_sets,
        task_properties = task_properties, packages = packages, man = man)
    }
  )
)
