#' @title Root Mean Squared Error Regression Measure
#'
#' @usage NULL
#' @aliases mlr_measures_regr.rmse
#' @format [R6::R6Class] inheriting from [MeasureClassif].
#' @include MeasureRegr.R
#'
#' @section Construction:
#' ```
#' MeasureRegrRMSE$new()
#' mlr_measures$get("regr.rmse")
#' msr("regr.rmse")
#' ```
#'
#' @description
#' Calls [Metrics::rmse()].
#'
#' @template seealso_measure
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

#' @include mlr_measures.R
mlr_measures$add("regr.rmse", MeasureRegrRMSE)
