#' @title Elapsed Time Measure
#'
#' @format [R6::R6Class()] inheriting from [Measure].
#' @include Measure.R
#'
#' @description
#' Measures the elapsed time during train ("time_train"), predict ("time_predict"), or both ("time_both").
#'
#' @aliases mlr_measures_time_train
#'   mlr_measures_time_predict
#'   mlr_measures_time_both
#' @export
MeasureElapsedTime = R6Class("MeasureElapsedTime",
  inherit = Measure,
  public = list(
    parts = NULL,

    initialize = function(id = "elapsed_time", parts) {
      super$initialize(
        id = id,
        task_type = NA_character_,
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE
      )
      self$parts = assert_subset(parts, c("train", "predict"), empty.ok = FALSE)
    },

    score_internal = function(prediction, learner, ...) {
      parts = c("train", "predict")
      sum(unlist(learner$data[sprintf("%s_time", parts)][parts %in% self$parts]))
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("time_train", MeasureElapsedTime, id = "time_train", parts = "train")
mlr_measures$add("time_predict", MeasureElapsedTime, id = "time_predict", parts = "predict")
mlr_measures$add("time_both", MeasureElapsedTime, id = "time_both", parts = c("train", "predict"))
