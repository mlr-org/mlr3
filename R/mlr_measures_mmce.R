#' @include MeasureClassif.R
MeasureClassifMMCE = R6Class("MeasureClassifMMCE",
  inherit = MeasureClassif,
  public = list(
    initialize = function(id = "mmce") {
      super$initialize(
        id = id,
        range = 0:1,
        minimize = TRUE,
        packages = "measures"
      )
    },

    calculate = function(experiment) {
      p = experiment$prediction
      measures::MMCE(p$truth, p$response)
    }
  )
)
