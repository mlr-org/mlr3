#' @title Classification Error Measure
#'
#' @aliases mlr_measures_classif.ce
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Calls [Metrics::ce()].
#'
#' @export
MeasureClassifCE = R6Class("MeasureClassifCE",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.ce",
        range = 0:1,
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    calculate = function(experiment = NULL, prediction = experiment$prediction) {
      Metrics::ce(actual = prediction$truth, predicted = prediction$response)
    })
)
