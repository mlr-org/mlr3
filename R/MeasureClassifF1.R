#' @title F1 Classification Measure
#'
#' @name mlr_measures_classif.f1
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Calls [Metrics::f1()].
#'
#' @export
MeasureClassifF1 = R6Class("MeasureClassifF1",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.auc",
        range = 0:1,
        minimize = FALSE,
        predict_type = "prob",
        task_properties = "twoclass",
        packages = "Metrics"
      )
    },

    calculate = function(experiment = NULL, prediction = experiment$prediction) {
      Metrics::f1(actual = prediction$truth, predicted = prediction$response)
    }
  )
)
