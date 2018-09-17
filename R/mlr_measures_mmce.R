#' @include Measure.R
MeasureMMCE = R6Class("MeasureMMCE",
  inherit = Measure,
  public = list(
    initialize = function(id = "mmce") {
      super$initialize(
        id = id,
        task_types = "classif",
        range = 0:1,
        minimize = TRUE,
        packages = "measures"
      )
    },

    calculate = function(experiment) {
      p = experiment$predictions
      measures::MMCE(p$truth, p$predicted)
    }
  )
)
