#' @title Average Pinball Loss
#'
#' @name mlr_measures_regr.pinball
#' @include Measure.R
#'
#' @description
#' Measure to compare true observed response with predicted response in regression tasks.
#'
#' @details
#' The pinball loss for quantile regression is defined as \deqn{
#' \text{Average Pinball Loss} = \frac{1}{n} \sum_{i=1}^{n} w_{i}
#' \begin{cases}
#' q \cdot (t_i - r_i) & \text{if } t_i \geq r_i \\
#'(1 - q) \cdot (r_i - t_i) & \text{if } t_i < r_i
#' \end{cases}
#' }
#' where \eqn{q} is the quantile and \eqn{w_i} are normalized sample weights.
#'
#' @param alpha `numeric(1)`\cr
#' The quantile to compute the pinball loss.
#' Must be one of the quantiles that the Learner was trained on.
#
#' @templateVar id regr.pinball
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureRegrPinball = R6Class(
  "MeasureRegrPinball",
  inherit = MeasureRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(alpha = 0.5) {
      param_set = ps(alpha = p_dbl(lower = 0, upper = 1))
      param_set$set_values(alpha = alpha)
      super$initialize(
        id = "regr.pinball",
        param_set = param_set,
        predict_type = "quantiles",
        properties = "weights",
        minimize = TRUE,
        range = c(-Inf, Inf),
        label = "Average Pinball Loss",
        man = "mlr3::mlr_measures_regr.pinball"
      )
    }
  ),

  private = list(
    .score = function(prediction, weights = NULL, ...) {
      alpha = self$param_set$values$alpha

      mlr3measures::pinball(
        truth = prediction$truth,
        response = get_quantile_prediction(prediction, alpha, self$id),
        alpha = alpha,
        sample_weights = weights
      )
    }
  )
)

# Returns the predictions for quantile `alpha`.
# The quantiles are matched with a tolerance, because `seq(0.1, 0.9, by = 0.1)[3] == 0.3` is `FALSE`.
get_quantile_prediction = function(prediction, alpha, measure_id) {
  assert_number(alpha, lower = 0, upper = 1)
  quantiles = prediction$data$quantiles
  probs = attr(quantiles, "probs")
  i = which(abs(probs - alpha) < sqrt(.Machine$double.eps))
  if (length(i) != 1L) {
    error_input(
      "Measure '%s' needs predictions for the quantile %s, but the prediction only contains the quantiles %s",
      measure_id,
      format(alpha),
      str_collapse(format(probs), quote = "'")
    )
  }
  quantiles[, i]
}

#' @include mlr_measures.R
mlr_measures$add("regr.pinball", MeasureRegrPinball)
