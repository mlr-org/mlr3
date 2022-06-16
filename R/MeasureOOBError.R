#' @title Out-of-bag Error Measure
#'
#' @name mlr_measures_oob_error
#' @include Measure.R
#'
#' @description
#' Returns the out-of-bag error of the [Learner] for learners that support it
#' (learners with property `"oob_error"`).
#' Returns `NA` for unsupported learners.
#'
#' @templateVar id oob_error
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureOOBError = R6Class("MeasureOOBError",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "oob_error",
        measure_type = NA_character_,
        properties = c("na_score", "requires_learner"),
        predict_type = "response",
        range = c(-Inf, Inf),
        minimize = TRUE,
        label = "Out-of-bag Error",
        man = "mlr3::mlr_measures_oob_error"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      learner = learner$base_learner()
      if ("oob_error" %nin% learner$properties) {
        return(NA_real_)
      }

      return(learner$oob_error())
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("oob_error", function() MeasureOOBError$new())
