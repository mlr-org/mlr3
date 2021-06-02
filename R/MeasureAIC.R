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
#' @section Parameters:
#' `r rd_info(msr("aic")$param_set)`
#'
#' `k` is the penalty to be used in [stats::AIC()] (defaulting to 2).
#'
#' @section Meta Information:
#' * Type: `NA`
#' * Range: \eqn{(-\infty, \infty)}{(-Inf, Inf)}
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
      param_set = ps(k = p_int(lower = 0))
      super$initialize(
        id = "aic",
        task_type = NA_character_,
        param_set = param_set,
        properties = c("na_score", "requires_learner", "requires_model"),
        predict_type = "response",
        minimize = TRUE,
        man = "mlr3::mlr_measures_aic"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      learner = learner$base_learner()
      if ("loglik" %nin% learner$properties) {
        return(NA_real_)
      }

      k = self$param_set$values$k %??% 2
      return(stats::AIC(learner$loglik(), k = k))
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("aic", MeasureAIC)
