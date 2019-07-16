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
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.ce",
        range = 0:1,
        minimize = TRUE,
        packages = "Metrics"
      )
    },

    score_internal = function(prediction, ...) {
      Metrics::ce(actual = prediction$truth, predicted = prediction$response)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("classif.ce", MeasureClassifCE)
