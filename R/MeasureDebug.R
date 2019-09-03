#' @title Debug Measure
#'
#' @usage NULL
#' @aliases mlr_measures_debug
#' @format [R6::R6Class()] inheriting from [Measure].
#' @include Measure.R
#'
#' @section Construction:
#' ```
#' MeasureDebug$new()
#' mlr_measures$get("debug")
#' msr("debug")
#' ```
#'
#' @description
#' This measure returns the number of observations in the [Prediction] object.
#' Its main purpose is debugging.
#'
#' @template seealso_measure
#' @export
MeasureDebug = R6Class("MeasureDebug",
  inherit = Measure,
  public = list(
    initialize = function() {
      super$initialize(
        id = "debug",
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
mlr_measures$add("debug", MeasureDebug)
