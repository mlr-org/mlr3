#' @include MeasureClassif.R
MeasureClassifACC = R6Class("MeasureClassifACC",
  inherit = MeasureClassif,
  public = list(
    initialize = function(id = "acc") {
      super$initialize(
        id = id,
        range = 0:1,
        minimize = FALSE,
        packages = "measures"
      )
    },

    calculate = function(experiment) {
      p = experiment$prediction
      measures::ACC(p$truth, p$response)
    }
  )
)
