#' @include Measure.R
MeasureRegr = R6Class("MeasureRegr", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize, predict_type = "response", task_properties = character(0L), learner_properties = character(0L), packages = character(0L)) {
      super$initialize(id, task_type = "regr", range = range, minimize = minimize, predict_type = predict_type,
        task_properties = task_properties, learner_properties = learner_properties, packages = packages)
    }
  )
)
