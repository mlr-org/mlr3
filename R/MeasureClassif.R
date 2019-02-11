#' @title Classification Measure
#'
#' @name MeasureClassif
#' @format [R6::R6Class] object inheriting from [Measure].
#' @description
#' This task specializes [Measure] for classification problems.
#'
#' @section Usage:
#' See [Measure].
#'
#' @section Details:
#' `$task_type` is `"classif"`.
#'
#' @family Measure
NULL

#' @include Measure.R
#' @export
MeasureClassif = R6Class("MeasureClassif", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L)) {
      super$initialize(id, task_type = "classif",  range = range, minimize = minimize,
        predict_type = predict_type, task_properties = task_properties, packages = packages)
    }
  )
)
