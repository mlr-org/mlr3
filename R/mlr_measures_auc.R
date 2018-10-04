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

    calculate = function(experiment) {
      cl = experiment$data$task$all_classes
      p = experiment$prediction
      measures::AUC(p[[paste0("prob.", cl[1L])]], p$truth, cl[2L], cl[1L])
    }
  )
)

