#' @title Classification Error Measure
#'
#' @usage NULL
#' @aliases mlr_measures_classif.ce
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @section Construction:
#' ```
#' MeasureClassifCE$new()
#' mlr_measures$get("classif.ce")
#' msr("classif.ce")
#' ```
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
