#' @title Featureless Regression Learner
#'
#' @name mlr_learners_regr.featureless
#' @include LearnerRegr.R
#'
#' @description
#' A simple [LearnerRegr] which only analyzes the response during train, ignoring all features.
#' If hyperparameter `robust` is `FALSE` (default), constantly predicts `mean(y)` as response
#' and `sd(y)` as standard error.
#' If `robust` is `TRUE`, [median()] and [mad()] are used instead of [mean()] and [sd()],
#' respectively.
#'
#' For weighted data, the response is the weighted mean (weighted median for robust regression).
#' The predicted standard error is the square root of the weighted variance estimator with bias correction
#' based on effective degrees of freedom:
#' ```
#' sd(y, weights) = sqrt(
#'   sum(weights * (y - weighted.mean(y, weights))^2) /
#'     (sum(weights) - sum(weights ^2) / sum(weights))
#' )
#' ```
#' If `robust` is `TRUE`, the weighted median absolute deviation is used, adjusted by a factor of 1.4826
#' for consistency with [mad()].
#'
#' @templateVar id regr.featureless
#' @template learner
#'
#' @template seealso_learner
#' @export
LearnerRegrFeatureless = R6Class("LearnerRegrFeatureless", inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        robust = p_lgl(default = TRUE, tags = "train")
      )
      ps$set_values(robust = FALSE)

      super$initialize(
        id = "regr.featureless",
        feature_types = unname(mlr_reflections$task_feature_types),
        predict_types = c("response", "se", "quantiles"),
        param_set = ps,
        properties = c("featureless", "missings", "importance", "selected_features", "weights"),
        packages = "stats",
        label = "Featureless Regression Learner",
        man = "mlr3::mlr_learners_regr.featureless"
      )
    },


    #' @description
    #' All features have a score of `0` for this learner.
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      fn = self$model$features
      named_vector(fn, 0)
    },

    #' @description
    #' Selected features are always the empty set for this learner.
    #' @return `character(0)`.
    selected_features = function() {
      character()
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      x = task$data(cols = task$target_names)[[1L]]
      weights = private$.get_weights(task)
      quantiles = if (self$predict_type == "quantiles") {
        if (is.null(private$.quantiles) || is.null(private$.quantile_response)) {
          stopf("Quantiles '$quantiles' and response quantile '$quantile_response' must be set")
        }
        quantile_weighted(x, probs = private$.quantiles, weights = weights)
      }

      if (isFALSE(pv$robust)) {
        wmd = weighted_mean_sd(x, weights)
        location = wmd$mean
        dispersion = wmd$sd
      } else {
        location = quantile_weighted(x, probs = 0.5, weights = weights, continuous = FALSE)
        dispersion = quantile_weighted(abs(x - location), probs = 0.5, weights = weights, continuous = FALSE) * 1.4826
      }

      set_class(list(
        location = location,
        dispersion = dispersion,
        quantiles = quantiles,
        features = task$feature_names), "regr.featureless_model")
    },

    .predict = function(task) {
      n = task$nrow

      if (self$predict_type == "quantiles") {
        quantiles = matrix(rep(self$model$quantiles, n), nrow = n, byrow = TRUE)
        setattr(quantiles, "probs", private$.quantiles)
        setattr(quantiles, "response", private$.quantile_response)
        return(list(quantiles = quantiles))
      }

      response = rep(self$model$location, n)
      se = if (self$predict_type == "se") rep(self$model$dispersion, n) else NULL
      list(response = response, se = se)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.featureless", function() LearnerRegrFeatureless$new())
