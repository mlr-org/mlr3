#' @title Mean Squared Error Regression Measure
#'
#' @usage NULL
#' @aliases mlr_measures_regr.mse
#' @format [R6::R6Class] inheriting from [MeasureRegr].
#' @include MeasureRegr.R
#'
#' @section Construction:
#' ```
#' MeasureRegrMSE$new()
#' mlr_measures$get("regr.mse")
#' msr("regr.mse")
#' ```
#'
#' @description
#' Calls [Metrics::mse()].
#'
#' @template seealso_measure
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

#' @include mlr_measures.R
mlr_measures$add("regr.mse", MeasureRegrMSE)
