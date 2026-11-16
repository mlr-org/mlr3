#' @title Elapsed Time Measure
#'
#' @name mlr_measures_elapsed_time
#' @aliases mlr_measures_time_train mlr_measures_time_predict mlr_measures_time_both
#' @include Measure.R
#'
#' @description
#' Measures the elapsed time during train ("time_train"), predict ("time_predict"), or both ("time_both").
#' Aggregation of elapsed time defaults to mean but can be configured via the field `aggregator` of the [Measure].
#'
#' When predictions for multiple predict sets were made during [resample()] or [benchmark()],
#' the predict time shows the cumulative duration of all predictions.
#' If `learner$predict()` is called manually, the last predict time gets overwritten.
#' The elapsed time accounts only for the training duration of the primary learner,
#' excluding the time required for training the fallback learner.
#' Returns `NA` if the time of one of the stages is not available,
#' e.g. because the learner was not trained or did not predict yet.
#'
#' @template param_id
#' @templateVar id time_train
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureElapsedTime = R6Class(
  "MeasureElapsedTime",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param stages (`character()`)\cr
    #'   Subset of `("train", "predict")`.
    #'   The runtime of provided stages will be summed.
    initialize = function(id = "elapsed_time", stages) {
      super$initialize(
        id = id,
        task_type = NA_character_,
        predict_sets = NULL,
        predict_type = NA_character_,
        range = c(0, Inf),
        minimize = TRUE,
        properties = c("na_score", "requires_learner", "requires_no_prediction"),
        label = "Elapsed Time",
        man = "mlr3::mlr_measures_elapsed_time"
      )
      self$stages = stages
    }
  ),

  active = list(
    #' @field stages (`character()`)\cr
    #' Which stages of the learner to measure?
    #' Usually set during construction.
    stages = function(rhs) {
      if (missing(rhs)) {
        return(private$.stages)
      }
      private$.stages = assert_subset(rhs, c("train", "predict"), empty.ok = FALSE)
    }
  ),

  private = list(
    .stages = NULL,

    .score = function(prediction, learner, ...) {
      times = unlist(learner$state[sprintf("%s_time", self$stages)], use.names = FALSE)
      if (length(times) < length(self$stages)) {
        return(NA_real_)
      }
      sum(times)
    },

    .extra_hash = "stages"
  )
)

#' @include mlr_measures.R
mlr_measures$add("time_train", function() MeasureElapsedTime$new(id = "time_train", stages = "train"))
mlr_measures$add("time_predict", function() MeasureElapsedTime$new(id = "time_predict", stages = "predict"))
mlr_measures$add("time_both", function() MeasureElapsedTime$new(id = "time_both", stages = c("train", "predict")))
