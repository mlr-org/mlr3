#' @title Number of Observations in Prediction Measure
#'
#' @usage NULL
#' @aliases mlr_measures_n_predict
#' @format [R6::R6Class()] inheriting from [Measure].
#' @include Measure.R
#'
#' @section Construction:
#' ```
#' MeasureNPredict$new()
#' mlr_measures$get("n_predict")
#' msr("n_predict")
#' ```
#'
#' @description
#' Returns the number of observations in the [Prediction] object.
#' Its main purpose is debugging.
#'
#' @template seealso_measure
#' @export
MeasureNPredict = R6Class("MeasureNPredict",
  inherit = Measure,
  public = list(
    parts = NULL,

    initialize = function() {
      super$initialize(
        id = "n_predict",
        task_type = NA_character_,
        predict_type = "response",
        range = c(0, Inf),
        minimize = NA
      )
    },

    score_internal = function(prediction, ...) {
      length(prediction$row_ids)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("n_predict", MeasureNPredict)
