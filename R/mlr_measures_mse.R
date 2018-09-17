#' @include Measure.R
MeasureMSE = R6Class("MeasureMSE",
  inherit = Measure,
  public = list(
    initialize = function(id = "mse") {
      super$initialize(
        id = id,
        task_types = "regr",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "measures"
      )
    },

    calculate = function(experiment) {
      p = experiment$predictions
      measures::MSE(p$truth, p$predicted)
    }
  )
)
