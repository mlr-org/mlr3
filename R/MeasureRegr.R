#' @include Measure.R
MeasureRegr = R6Class("MeasureRegr", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize, packages = character(0L)) {
      super$initialize(id, task_type = "regr", range, minimize, packages)
    }
  )
)

