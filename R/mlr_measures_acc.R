#' @include Measure.R
MeasureACC = R6Class("MeasureACC", inherit = Measure,
  public = list(
    id = "acc",
    description = "Accuracy",
    task_types = "TaskClassif",
    fun = function(truth, predicted) {
      mean(truth == predicted)
    }
  )
)
