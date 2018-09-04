#' @include Measure.R
MeasureMSE = R6Class("MeasureMSE", inherit = Measure,
  public = list(
    id = "mse",
    task_types = "TaskRegr",
    fun = function(truth, predicted) {
      mean( (truth - predicted)^2 )
    }
  )
)
