#' @title Classification Measure
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Measure].
#' @include Measure.R
#'
#' @description
#' This measure specializes [Measure] for classification problems.
#' Predefined measures can be found in the [mlr3misc::Dictionary] [mlr_measures].
#'
#' @section Construction:
#' ```
#' m = MeasureClassif$new(id, range, minimize = NA, aggregator = NULL, properties = character(), predict_type = "response",
#'     predict_sets = "test", task_properties = character(), packages = character(), man = NA_character_)
#' ```
#' For a description of the arguments, see [Measure].
#' The `task_type` is set to `"classif"`.
#' Possible values for `predict_type` are `"response"` and `"prob"`.
#'
#' @section Fields:
#' See [Measure].
#'
#' @section Methods:
#' See [Measure].
#'
#' @family Measure
#' @seealso
#' Example classification measures: [`classif.ce`][mlr_measures_classif.ce]
#' @export
MeasureClassif = R6Class("MeasureClassif", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize = NA, aggregator = NULL, properties = character(), predict_type = "response",
      predict_sets = "test", task_properties = character(), packages = character(), man = NA_character_) {
      super$initialize(id, task_type = "classif", range = range, minimize = minimize, aggregator = aggregator,
        properties = properties, predict_type = predict_type, predict_sets = predict_sets,
        task_properties = task_properties, packages = packages, man = man)
    }
  )
)
