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
        packages = "measures",
      )
    },

    calculate = function(e) {
      p = e$prediction
      task = e$data$task
      measures::AUC(p[[paste0("prob.", task$positive)]], p$truth, task$negative, task$positive)
    }
  )
)


#' @include mlr_measures.R
mlr_measures$add("auc", MeasureClassifAUC)
