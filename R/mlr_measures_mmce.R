#' @include Measure.R
MeasureMMCE = R6Class("MeasureMMCE", inherit = Measure,
  public = list(
    id = "mmce",
    description = "Mean misclassification error",
    task_types = "classif",
    fun = function(truth, predicted) {
      mean(truth != predicted)
    }
  )
)
