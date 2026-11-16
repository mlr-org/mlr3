#' @title Bayesian Information Criterion Measure
#'
#' @name mlr_measures_bic
#' @include Measure.R
#'
#' @description
#' Calculates the Bayesian Information Criterion (BIC) which is a
#' trade-off between goodness of fit (measured in terms of
#' log-likelihood) and model complexity (measured in terms of number
#' of included features).
#' Internally, [stats::BIC()] is called.
#' Requires that a [stats::logLik()] method exists for the model of the learner.
#' For other learners, `NA` is returned with a warning.
#'
#' @templateVar id bic
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureBIC = R6Class(
  "MeasureBIC",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "bic",
        task_type = NA_character_,
        properties = c("na_score", "requires_learner", "requires_model", "requires_no_prediction"),
        predict_sets = NULL,
        predict_type = NA_character_,
        minimize = TRUE,
        label = "Bayesian Information Criterion",
        man = "mlr3::mlr_measures_bic"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      loglik = learner_loglik(learner$base_learner(), "BIC")
      if (is.null(loglik)) {
        return(NA_real_)
      }
      stats::BIC(loglik)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("bic", function() MeasureBIC$new())
