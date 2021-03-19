#' @title Akaike Information Criterion Measure
#'
#' @name mlr_measures_aic
#' @include Measure.R
#'
#' @description
#' Calculates the Akaike Information Criterion (AIC) which is a
#' trade-off between goodness of fit (measured in terms of
#' log-likelihood) and model complexity (measured in terms of number
#' of included features).
#' Internally, [stats::AIC()] is called.
#' Requires the learner property `"loglik"`, `NA` is returned for unsupported learners.
#'
#' @templateVar id aic
#' @template section_dictionary_measure
#'
#' @section Meta Information:
#' * Type: `NA`
#' * Range: \eqn{[0, \infty)}{[0, Inf)}
#' * Minimize: `TRUE`
#' * Required prediction: 'response'
#' * Learner Property: `loglik`
#'
#' @template seealso_measure
#' @export
MeasureAIC = R6Class("MeasureAIC",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "aic",
        task_type = NA_character_,
        properties = c("na_score", "requires_learner", "requires_model"),
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        man = "mlr3::mlr_measures_aic"
      )
    },

    #' @field k (`integer(1)`):\cr
    #' Penalty to use. See [stats::AIC()].
    k = 2L
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      if ("loglik" %nin% learner$properties) {
        return(NA_real_)
      }
      stats::AIC(learner$loglik(), k = self$k)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("aic", MeasureAIC)
