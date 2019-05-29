#' @title Area Under the Curve Classification Measure
#'
#' @aliases mlr_measures_classif.auc
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Calls [Metrics::auc()].
#'
#' @export
MeasureClassifAUC = R6Class("MeasureClassifAUC",
  inherit = MeasureClassif,
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
      truth = prediction$truth
      positive = levels(truth)[1L]
      Metrics::auc(actual = as.integer((truth == positive)), predicted = prediction$prob[, positive])
    }
  )
)
