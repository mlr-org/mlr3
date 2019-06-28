#' @title Absolute Errors Regression Measure
#'
#' @aliases mlr_measures_regr.mae
#' @format [R6::R6Class] inheriting from [MeasureClassif].
#' @include MeasureRegr.R
#'
#' @description
#' Calls [Metrics::mae()].
#'
#' @export
MeasureRegrMAE = R6Class("MeasureRegrMAE",
  inherit = MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.mae",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    score_internal = function(prediction, ...) {
      Metrics::mae(actual = prediction$truth, predicted = prediction$response)
    }
  )
)
