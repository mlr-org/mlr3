#' @include Measure.R
MeasureMSE = R6Class("MeasureMSE", inherit = Measure,
  public = list(
    id = "mse",
    description = "Mean squared error",
    task_types = "TaskRegr",
    fun = function(truth, predicted) {
      mean( (truth - predicted)^2 )
    }
  )
)
