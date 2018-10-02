#' @include Measure.R
MeasureElapsedTime = R6Class("MeasureElapsedTime",
  inherit = Measure,
  public = list(
    parts = NULL,

    initialize = function(id = NA_character_, parts) {
      super$initialize(
        id = id,
        task_type = NA_character_,
        predict_type = NA_character_,
        range = c(0, Inf),
        minimize = TRUE
      )
      self$parts = assert_subset(parts, c("train", "predict"), empty.ok = FALSE)
    },

    calculate = function(experiment) {
      sum(experiment$timings[self$parts], na.rm = TRUE)
    }
  )
)
