#' @title Mean Squared Error Measure
#'
#' @name mlr_measures_mse
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#'
#' @description
#' Calls [Metrics::mse].
#'
#' @export
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/mlr_measures_mse.html)
#' @include MeasureRegr.R
MeasureRegrMSE = R6Class("MeasureRegrMSE",
  inherit = MeasureRegr,
  public = list(
    initialize = function(id = "mse") {
      super$initialize(
        id = id,
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
mlr_measures$add("mse", MeasureRegrMSE)
