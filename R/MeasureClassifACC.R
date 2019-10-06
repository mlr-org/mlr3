#' @title Accuracy Classification Measure
#'
#' @usage NULL
#' @aliases mlr_measures_classif.acc
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @section Construction:
#' ```
#' MeasureClassifACC$new()
#' mlr_measures$get("classif.acc")
#' msr("classif.acc")
#' ```
#'
#' @description
#' Calls [Metrics::accuracy()].
#'
#' @template seealso_measure
#' @export
MeasureClassifACC = R6Class("MeasureClassifACC",
  inherit = MeasureClassif,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.acc",
        range = 0:1,
        minimize = FALSE,
        packages = "Metrics",
        man = "mlr3::mlr_measures_classif.acc"
      )
    },

    score_internal = function(prediction, ...) {
      Metrics::accuracy(actual = prediction$truth, predicted = prediction$response)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("classif.acc", MeasureClassifACC)
