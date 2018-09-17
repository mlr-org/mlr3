#' @include Measure.R
MeasureACC = R6Class("MeasureACC",
  inherit = Measure,
  public = list(
    initialize = function(id = "acc") {
      super$initialize(
        id = id,
        task_types = "classif",
        range = 0:1,
        minimize = FALSE,
        packages = "measures"
      )
    },

    calculate = function(experiment) {
      p = experiment$predictions
      measures::ACC(p$truth, p$predicted)
    }
  )
)
