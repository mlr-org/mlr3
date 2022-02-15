#' @title Classification Measure
#'
#' @include Measure.R
#'
#' @description
#' This measure specializes [Measure] for classification problems:
#'
#' * `task_type` is set to `"classif"`.
#' * Possible values for `predict_type` are `"response"` and `"prob"`.
#'
#' Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#' The default measure for classification is [`classif.ce`][mlr_measures_classif.ce].
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
MeasureClassif = R6Class("MeasureClassif", inherit = Measure, cloneable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), range, minimize = NA, average = "macro", aggregator = NULL, properties = character(), predict_type = "response",
      predict_sets = "validation", task_properties = character(), packages = character(), man = NA_character_) {
      super$initialize(id, task_type = "classif", param_set = param_set, range = range, minimize = minimize, average = average, aggregator = aggregator,
        properties = properties, predict_type = predict_type, predict_sets = predict_sets,
        task_properties = task_properties, packages = packages, man = man)
    }
  )
)
