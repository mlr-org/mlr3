#' @include MeasureRegr.R
MeasureRegrMSE = R6Class("MeasureRegrMSE",
  inherit = MeasureRegr,
  public = list(
    initialize = function(id = "mse") {
      super$initialize(
        id = id,
        range = c(0, Inf),
        minimize = TRUE,
        packages = "measures"
      )
    },

    calculate = function(experiment) {
      p = experiment$prediction
      measures::MSE(p$truth, p$response)
    }
  )
)
