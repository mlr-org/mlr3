#' @title Regression Measure
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Measure].
#' @include Measure.R
#'
#' @description
#' This measure specializes [Measure] for regression problems.
#' Predefined measures can be found in the [Dictionary] [mlr_measures].
#'
#' @section Construction:
#' ```
#' m = MeasureRegr$new(id, range, minimize, predict_type = "response",
#'      task_properties = character(0L), packages = character(0L))
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
#' @export
MeasureRegr = R6Class("MeasureRegr", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize = NA, aggregator = NULL, predict_type = "response", task_properties = character(0L), na_score = FALSE, packages = character(0L)) {
      super$initialize(id, task_type = "regr", range = range, minimize = minimize, aggregator = aggregator,
        predict_type = predict_type, task_properties = task_properties, na_score = na_score, packages = packages)
    })
)
