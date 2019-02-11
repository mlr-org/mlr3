#' @title Mean Squared Error Measure
#'
#' @name mlr_measures_regr_mse
#' @format [R6::R6Class] inheriting from [MeasureClassif].
#'
#' @description
#' Calls [Metrics::mse].
#'
#' @export
#' @include MeasureRegr.R
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


#' @include mlr_measures.R
mlr_measures$add("regr.mse", MeasureRegrMSE)
