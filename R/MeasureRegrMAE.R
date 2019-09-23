#' @title Absolute Errors Regression Measure
#'
#' @usage NULL
#' @aliases mlr_measures_regr.mae
#' @format [R6::R6Class] inheriting from [MeasureRegr].
#' @include MeasureRegr.R
#'
#' @section Construction:
#' ```
#' MeasureRegrMAE$new()
#' mlr_measures$get("regr.mae")
#' msr("regr.mae")
#' ```
#'
#' @description
#' Calls [Metrics::mae()].
#'
#' @template seealso_measure
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

#' @include mlr_measures.R
mlr_measures$add("regr.mae", MeasureRegrMAE)
