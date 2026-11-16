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
#' Requires that a [stats::logLik()] method exists for the model of the learner.
#' For other learners, `NA` is returned with a warning.
#'
#' @templateVar id aic
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureAIC = R6Class(
  "MeasureAIC",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(k = p_dbl(lower = 0))
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
      loglik = learner_loglik(learner$base_learner(), "AIC")
      if (is.null(loglik)) {
        return(NA_real_)
      }
      stats::AIC(loglik, k = self$param_set$values$k %??% 2)
    }
  )
)

# Returns the log-likelihood of the model, or `NULL` with a warning if the model has no `logLik()` method.
# Errors raised by an existing method are not caught.
learner_loglik = function(learner, criterion) {
  model = learner$model
  has_method = some(class(model), function(cl) !is.null(utils::getS3method("logLik", cl, optional = TRUE)))
  if (!has_method) {
    warning_config("Learner '%s' does not support %s calculation", learner$id, criterion)
    return(NULL)
  }
  stats::logLik(model)
}

#' @include mlr_measures.R
mlr_measures$add("aic", function() MeasureAIC$new())
