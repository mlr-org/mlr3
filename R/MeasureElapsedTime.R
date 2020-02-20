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
#'
#' @template param_id
#' @templateVar id time_train
#' @template section_dictionary_measure
#'
#'
#' @section Meta Information:
#' * Type: `NA`
#' * Range: \eqn{[0, \infty)}{[0, Inf)}
#' * Minimize: `TRUE`
#' * Required prediction: 'response'
#'
#' @template seealso_measure
#' @export
MeasureElapsedTime = R6Class("MeasureElapsedTime",
  inherit = Measure,
  public = list(

    #' @field stages (`character()`)\cr
    #' Which stages of the learner to measure?
    stages = NULL,

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
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        man = "mlr3::mlr_measures_elapsed_time"
      )
      self$stages = assert_subset(stages, c("train", "predict"), empty.ok = FALSE)
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      sum(unlist(learner$state[sprintf("%s_time", self$stages)]))
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("time_train", MeasureElapsedTime, id = "time_train", stages = "train")
mlr_measures$add("time_predict", MeasureElapsedTime, id = "time_predict", stages = "predict")
mlr_measures$add("time_both", MeasureElapsedTime, id = "time_both", stages = c("train", "predict"))
