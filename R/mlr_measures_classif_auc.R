#' @title Area Under the Curve Classification Measure
#'
#' @aliases mlr_measures_classif.auc
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Calls [Metrics::auc].
#'
#' @export
MeasureClassifAUC = R6Class("MeasureClassifAUC",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.auc",
        range = 0:1,
        minimize = FALSE,
        predict_type = "prob",
        packages = "Metrics",
      )
    },

    calculate = function(e) {
      p = e$prediction
      positive = e$data$task$positive
      Metrics::auc(actual = (p$truth == positive), predicted = p$prob[, positive])
    }
  )
)


#' @include mlr_measures.R
mlr_measures$add("classif.auc", MeasureClassifAUC)
