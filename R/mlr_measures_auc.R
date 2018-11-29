#' @include MeasureClassif.R
MeasureClassifAUC = R6Class("MeasureClassifAUC",
  inherit = MeasureClassif,
  public = list(
    initialize = function(id = "auc") {
      super$initialize(
        id = id,
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
mlr_measures$add("auc", MeasureClassifAUC)
