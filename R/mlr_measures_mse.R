#' @include Measure.R
MeasureMSE = R6Class("MeasureMSE",
  inherit = Measure,
  public = list(
    initialize = function(id = "mse") {
      super$initialize(
        id = id,
        task_types = "TaskRegr",
        range = c(0, Inf),
        minimize = TRUE
      )
    },

    calculate = function(experiment) {
      p = experiment$predictions
      mean( (p$truth - p$predicted)^2 )
    }
  )
)
