#' @include Measure.R
MeasureMMCE = R6Class("MeasureMMCE", inherit = Measure,
  public = list(
    initialize = function(id = "mmce") {
      super$initialize(
        id = id,
        task_types = "TaskClassif",
        range = 0:1,
        minimize = TRUE
      )
    },

    calculate = function(truth, predicted) {
      mean(truth != predicted)
    }
  )
)
