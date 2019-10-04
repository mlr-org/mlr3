#' @title Out-of-bag Error Measure
#'
#' @usage NULL
#' @aliases mlr_measures_oob_error
#' @format [R6::R6Class()] inheriting from [Measure].
#' @include Measure.R
#'
#' @section Construction:
#' ```
#' MeasureOOBError$new()
#' mlr_measures$get("oob_error")
#' msr("oob_error")
#' ```
#'
#' @description
#' Returns the out-of-bag error of the [Learner] for learners that support it
#' (learners with property `"oob_error"`).
#' Returns `NA` for unsupported learners.
#'
#' @template seealso_measure
#' @export
MeasureOOBError = R6Class("MeasureOOBError",
  inherit = Measure,
  public = list(
    parts = NULL,

    initialize = function() {
      super$initialize(
        id = "oob_error",
        task_type = NA_character_,
        properties = c("na_score", "requires_learner"),
        predict_type = "response",
        range = c(-Inf, Inf),
        minimize = NA,
        man = "mlr3::mlr_measures_oob_error"
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
