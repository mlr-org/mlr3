#' @include Measure.R
MeasureElapsedTime = R6Class("MeasureElapsedTime",
  inherit = Measure,
  public = list(
    parts = NULL,

    initialize = function(id = NA_character_, parts) {
      super$initialize(
        id = id,
        task_type = NA_character_,
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE
      )
      self$parts = assert_subset(parts, c("train", "predict"), empty.ok = FALSE)
    },

    calculate = function(e) {
      sum(e$timings[self$parts], na.rm = TRUE)
    }
  )
)


#' @include mlr_measures.R
mlr_measures$add("time_train", MeasureElapsedTime$new("time_train", "train"))
mlr_measures$add("time_predict", MeasureElapsedTime$new("time_predict", "predict"))
mlr_measures$add("time_both", MeasureElapsedTime$new("time_both", c("train", "predict")))
