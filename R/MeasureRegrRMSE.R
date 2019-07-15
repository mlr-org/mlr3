#' @title Root Mean Squared Error Regression Measure
#'
#' @aliases mlr_measures_regr.rmse
#' @format [R6::R6Class] inheriting from [MeasureClassif].
#' @include MeasureRegr.R
#'
#' @description
#' Calls [Metrics::rmse()].
#'
#' @export
MeasureRegrRMSE = R6Class("MeasureRegrRMSE",
  inherit = MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.rmse",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    score_internal = function(prediction, ...) {
      Metrics::rmse(actual = prediction$truth, predicted = prediction$response)
    }
  )
)
