#' @title Elapsed Time Measure
#'
#' @name mlr_measures_elapsed_time
#' @aliases
#'   mlr_measures_time_train
#'   mlr_measures_time_predict
#'   mlr_measures_time_both
#' @include Measure.R
#'
#' @description
#' Measures the elapsed time during train ("time_train"), predict ("time_predict"), or both ("time_both").
#' Aggregation of elapsed time defaults to mean but can be configured via the field `aggregator` of the [Measure].
#'
#' When predictions for multiple predict sets were made during [resample()] or [benchmark()], the predict time shows the cumulative duration of all predictions.
#' If `learner$predict()` is called manually, the last predict time gets overwritten.
#' The elapsed time accounts only for the training duration of the primary learner, excluding the time required for training the fallback learner.
#'
#' @template param_id
#' @templateVar id time_train
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureElapsedTime = R6Class("MeasureElapsedTime",
  inherit = Measure,
  public = list(

    #' @field stages (`character()`)\cr
    #' Which stages of the learner to measure?
    #' Usually set during construction.
    stages = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param stages (`character()`)\cr
    #'   Subset of `("train", "predict")`.
    #'   The runtime of provided stages will be summed.
    initialize = function(stages) {
      super$initialize(
        id = "elapsed_time",
        task_type = NA_character_,
        predict_sets = NULL,
        predict_type = NA_character_,
        range = c(0, Inf),
        minimize = TRUE,
        properties = c("requires_learner", "requires_no_prediction")
      )
      self$stages = assert_subset(stages, c("train", "predict"), empty.ok = FALSE)
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      sum(unlist(learner$state[sprintf("%s_time", self$stages)], use.names = FALSE))
    },

    .additional_phash_input = function() {
      c(list(self$stages), super$.additional_phash_input())
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("time_train", function() MeasureElapsedTime$new(stages = "train")$configure(id = "time_train"))
mlr_measures$add("time_predict", function() MeasureElapsedTime$new(stages = "predict")$configure(id = "time_predict"))
mlr_measures$add("time_both", function() MeasureElapsedTime$new(stages = c("train", "predict"))$configure(id = "time_both"))
mlr_measures$add("elapsed_time", MeasureElapsedTime, .prototype_args = list(stages = "train"))
