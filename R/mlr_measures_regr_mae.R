#' @title Absolute Errors Regression Measure
#'
#' @name mlr_measures_regr.mae
#' @format [R6::R6Class] inheriting from [MeasureClassif].
#' @include MeasureRegr.R
#'
#' @description
#' Calls [Metrics::mae()].
#'
#' @export
MeasureRegrMAE = R6Class("MeasureRegrMAE",
  inherit = MeasureRegr,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.mae",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    calculate = function(e) {
      p = e$prediction
      Metrics::mae(actual = p$truth, predicted = p$response)
    }
  )
)
