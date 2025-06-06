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
#' Internally, [stats::AIC()] is called with parameter `k` (defaulting to 2).
#' Requires the learner property `"loglik"`, `NA` is returned for unsupported learners.
#'
#' @templateVar id aic
#' @template measure
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
        predict_sets = NULL,
        properties = c("na_score", "requires_learner", "requires_model", "requires_no_prediction"),
        predict_type = NA_character_,
        minimize = TRUE,
        label = "Akaike Information Criterion",
        man = "mlr3::mlr_measures_aic"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      learner = learner$base_learner()
      k = self$param_set$values$k %??% 2

      tryCatch({
        return(stats::AIC(stats::logLik(learner$model), k = k))
      }, error = function(e) {
        warningf("Learner '%s' does not support AIC calculation", learner$id)
        return(NA_real_)
      })
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("aic", function() MeasureAIC$new())
