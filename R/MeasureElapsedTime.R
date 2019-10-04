#' @title Elapsed Time Measure
#'
#' @usage NULL
#' @aliases mlr_measures_elapsed_time
#'   mlr_measures_time_train
#'   mlr_measures_time_predict
#'   mlr_measures_time_both
#' @format [R6::R6Class()] inheriting from [Measure].
#' @include Measure.R
#'
#' @section Construction:
#' ```
#' MeasureElapsedTime$new(id, stages)
#'
#' mlr_measures$get("time_train")
#' mlr_measures$get("time_predict")
#' mlr_measures$get("time_both")
#'
#' msr$get("time_train")
#' msr$get("time_predict")
#' msr$get("time_both")
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Id for the created measure.
#' * `stages` :: `character()`\cr
#'   Subset of `("train", "predict")`.
#'   The runtime of all stages will be summed.
#'
#' @description
#' Measures the elapsed time during train ("time_train"), predict ("time_predict"), or both ("time_both").
#'
#' @template seealso_measure
#' @export
MeasureElapsedTime = R6Class("MeasureElapsedTime",
  inherit = Measure,
  public = list(
    stages = NULL,

    initialize = function(id = "elapsed_time", stages) {
      super$initialize(
        id = id,
        task_type = NA_character_,
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        man = "mlr3::mlr_measures_elapsed_time"
      )
      self$stages = assert_subset(stages, c("train", "predict"), empty.ok = FALSE)
    },

    score_internal = function(prediction, learner, ...) {
      sum(unlist(learner$state[sprintf("%s_time", self$stages)]))
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("time_train", MeasureElapsedTime, id = "time_train", stages = "train")
mlr_measures$add("time_predict", MeasureElapsedTime, id = "time_predict", stages = "predict")
mlr_measures$add("time_both", MeasureElapsedTime, id = "time_both", stages = c("train", "predict"))
