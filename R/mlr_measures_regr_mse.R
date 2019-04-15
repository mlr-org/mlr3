#' @title Mean Squared Error Regression Measure
#'
#' @name mlr_measures_regr.mse
#' @format [R6::R6Class] inheriting from [MeasureClassif].
#' @include MeasureRegr.R
#'
#' @description
#' Calls [Metrics::mse()].
#'
#' @export
MeasureRegrMSE = R6Class("MeasureRegrMSE",
  inherit = MeasureRegr,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.mse",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    calculate = function(e) {
      p = e$prediction
      Metrics::mse(actual = p$truth, predicted = p$response)
    }
  )
)
