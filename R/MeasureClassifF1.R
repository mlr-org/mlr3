#' @title F1 Classification Measure
#'
#' @usage NULL
#' @aliases mlr_measures_classif.f1
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @section Construction:
#' ```
#' MeasureClassifF1$new()
#' mlr_measures$get("classif.f1")
#' msr("classif.f1")
#' ```
#'
#' @description
#' Calls [Metrics::f1()].
#'
#' @template seealso_measure
#' @export
MeasureClassifF1 = R6Class("MeasureClassifF1",
  inherit = MeasureClassif,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.f1",
        range = 0:1,
        minimize = FALSE,
        predict_type = "prob",
        task_properties = "twoclass",
        packages = "Metrics"
      )
    },

    score_internal = function(prediction, ...) {
      Metrics::f1(actual = prediction$truth, predicted = prediction$response)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("classif.f1", MeasureClassifF1)
