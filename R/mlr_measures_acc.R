#' @include Measure.R
MeasureACC = R6Class("MeasureACC", inherit = Measure,
  public = list(
    initialize = function(id = "acc") {
      super$initialize(
        id = id,
        task_types = "TaskClassif",
        range = 0:1,
        minimize = FALSE
      )
    },

    calculate = function(e) {
      p = e$predictions
      mean(p$truth == p$predicted)
    }
  )
)
