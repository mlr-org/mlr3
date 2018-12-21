#' @title Regression Measure
#'
#' @format [R6Class] object
#' @description
#' This task specializes [Measure] for regression problems.
#'
#' @section Usage:
#' Inherits from [Measure].
#'
#' @section Details:
#' * `$task_type` is `"regr"`.
#'
#' @name MeasureRegr
#' @family Measure
NULL

#' @include Measure.R
#' @export
MeasureRegr = R6Class("MeasureRegr", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L)) {
      super$initialize(id, task_type = "regr", range = range, minimize = minimize, predict_type = predict_type,
        task_properties = task_properties, packages = packages)
    }
  )
)
