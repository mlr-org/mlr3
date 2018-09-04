#' @include Measure.R
MeasureMMCE = R6Class("MeasureMMCE", inherit = Measure,
  public = list(
    id = "mmce",
    task_types = "TaskClassif",
    fun = function(truth, predicted) {
      mean(truth != predicted)
    }
  )
)
