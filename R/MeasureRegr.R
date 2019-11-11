#' @title Regression Measure
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Measure].
#' @include Measure.R
#'
#' @description
#' This measure specializes [Measure] for regression problems.
#' Predefined measures can be found in the [mlr3misc::Dictionary] [mlr_measures].
#'
#' @section Construction:
#' ```
#' m = MeasureRegr$new(id, range, minimize = NA, average = "macro", aggregator = NULL, properties = character(), predict_type = "response",
#'     predict_sets = "test", task_properties = character(), packages = character())
#' ```
#' For a description of the arguments, see [Measure].
#' The `task_type` is set to `"regr"`.
#' Possible values for `predict_type` are `"response"` and `"se"`.
#'
#' @section Fields:
#' See [Measure].
#'
#' @section Methods:
#' See [Measure].
#'
#' @family Measure
#' @seealso
#' Example regression measures: [`regr.mse`][mlr_measures_regr.mse]
#' @export
MeasureRegr = R6Class("MeasureRegr", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize = NA, average = "macro", aggregator = NULL, properties = character(), predict_type = "response",
      predict_sets = "test", task_properties = character(), packages = character(), man = NA_character_) {
      super$initialize(id, task_type = "regr", range = range, minimize = minimize, average = average, aggregator = aggregator,
        properties = properties, predict_type = predict_type, predict_sets = predict_sets,
        task_properties = task_properties, packages = packages, man = man)
    }
  )
)
