#' @title R-Squared for Quantile Regression
#'
#' @name mlr_measures_regr.rqr
#' @include Measure.R
#'
#' @description
#' Measure to compare true observed response with predicted quantiles in regression tasks.
#'
#' @details
#' \eqn{R^1(\alpha)} is defined as \deqn{
#'   1 - \frac{\sum_{i=1}^n \rho_\alpha \left( t_i - r_i(\alpha) \right)}{\sum_{i=1}^n \rho_\alpha \left( t_i - q_{\alpha} \right)},
#' }{
#'   1 - sum(pinball(t - r(alpha))) / sum(pinball(t - quantile(t, alpha))),
#' }
#' where for a quantile \eqn{\alpha}, \eqn{\rho_\alpha} is the pinball function, \eqn{r_i(\alpha)} are the predictions
#' for the quantile and \eqn{q_{\alpha}} is the empirical \eqn{\alpha}-quantile of the test or training data.
#'
#' \eqn{R^1(\alpha)} is analogous to \eqn{R^2} for regression tasks.
#' It compares the pinball function of the predictions relative to a naive model predicting the empirical quantile.
#'
#' This measure is undefined for constant \eqn{t}.
#'
#' @param pred_set_mean `logical(1)`\cr
#' If `TRUE`, the mean of the true values is calculated on the prediction set.
#' If `FALSE`, the mean of the true values is calculated on the training set.
#'
#' @param alpha `numeric(1)`\cr
#' The quantile for which to compute the measure.
#' Must be one of the quantiles that the Learner was trained on.
#
#' @references
#' `r format_bib("koenker_1999")`
#'
#' @templateVar id regr.rqr
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureRegrRQR = R6Class("MeasureRQR",
  inherit = MeasureRegr,
  public = list(
   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function(alpha = 0.5, pred_set_mean = TRUE) {
     private$.pred_set_mean = assert_flag(pred_set_mean)
     param_set = ps(alpha = p_dbl(lower = 0, upper = 1))
     param_set$set_values(alpha = alpha)

     super$initialize(
       id = "regr.rqr",
       param_set = param_set,
       properties = c(if (!private$.pred_set_mean) c("requires_task", "requires_train_set")),
       predict_type = "quantiles",
       minimize = FALSE,
       range = c(-Inf, 1),
       man = "mlr3::mlr_measures_regr.rqr"
     )
   }
  ),

  private = list(
    .pred_set_mean = NULL,

    .score = function(prediction, task = NULL, train_set = NULL, weights = NULL, ...) {

      alpha = self$param_set$values$alpha
      probs = attr(prediction$data$quantiles, "probs")
      assert_choice(alpha, probs)

      mu = if (private$.pred_set_mean) {
        quantile(prediction$truth, probs = alpha)
      } else {
        quantile(task$truth(train_set), probs = alpha)
      }

      numerator = sum(
        mlr3measures::pinball(
          truth = prediction$truth,
          response = prediction$data$quantiles[, which(probs == alpha)],
          alpha = alpha
         )
      )

      denominator = sum(
        mlr3measures::pinball(
          truth = prediction$truth,
          response = rep(mu, length(prediction$truth)),
          alpha = alpha
        )
      )

       1 - (numerator / denominator)
     }
  )
)

#' @include mlr_measures.R
mlr_measures$add("regr.rqr", MeasureRegrRQR)
