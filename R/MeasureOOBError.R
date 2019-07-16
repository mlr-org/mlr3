#' @title Out-of-bag Error Measure
#'
#' @aliases mlr_measures_oob_error
#' @format [R6::R6Class()] inheriting from [Measure].
#' @include Measure.R
#'
#' @description
#' Returns the out-of-bag error of the learner for learners that support it
#' (these learners have property "oob_error").
#'
#' @export
MeasureOOBError = R6Class("MeasureOOBError",
  inherit = Measure,
  public = list(
    parts = NULL,

    initialize = function(id = "oob_error") {
      super$initialize(
        id = id,
        task_type = NA_character_,
        properties = "requires_learner",
        predict_type = "response",
        range = c(-Inf, Inf),
        minimize = NA,
        na_score = TRUE
      )
    },

    score_internal = function(prediction, learner, ...) {
      if ("oob_error" %nin% learner$properties) {
        return(NA_real_)
      }
      learner$oob_error()
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("oob_error", MeasureOOBError)
