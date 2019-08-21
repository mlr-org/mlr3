#' @title Area Under the Curve Classification Measure
#'
#' @usage NULL
#' @aliases mlr_measures_classif.auc
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @section Construction:
#' ```
#' MeasureClassifAUC$new()
#' mlr_measures$get("classif.auc")
#' msr("classif.auc")
#' ```
#'
#' @description
#' Calls [Metrics::auc()].
#'
#' @template seealso_measure
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

    score_internal = function(prediction, ...) {
      truth = prediction$truth
      positive = levels(truth)[1L]
      Metrics::auc(actual = as.integer((truth == positive)), predicted = prediction$prob[, positive])
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("classif.auc", MeasureClassifAUC)
