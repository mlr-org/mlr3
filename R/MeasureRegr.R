#' @include Measure.R
MeasureRegr = R6Class("MeasureRegr", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize, packages = character(0L), predict_type = NA_character_, properties = character(0L)) {
      super$initialize(id, task_type = "regr", range = range, minimize = minimize,
        packages = packages, predict_type = predict_type, properties = properties)
    }
  )
)

