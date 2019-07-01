#' @title Mean Squared Error Regression Measure
#'
#' @aliases mlr_measures_regr.mse
#' @format [R6::R6Class] inheriting from [MeasureClassif].
#' @include MeasureRegr.R
#'
#' @description
#' Calls [Metrics::mse()].
#'
#' @export
MeasureRegrMSE = R6Class("MeasureRegrMSE",
  inherit = MeasureRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.mse",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    score_internal = function(prediction, ...) {
      Metrics::mse(actual = prediction$truth, predicted = prediction$response)
    }
  )
)
