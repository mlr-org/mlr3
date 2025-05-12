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
MeasureRegrPinball = R6Class("MeasurePinball",
  inherit = MeasureRegr,
  public = list(
   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(alpha = 0.5) {
      param_set = ps(alpha = p_dbl(lower = 0, upper = 1))
      param_set$set_values(alpha = alpha)
      super$initialize(
        id = "pinball",
        param_set = param_set,
        predict_type = "quantiles",
        minimize = TRUE,
        range = c(-Inf, Inf),
        man = "mlr3::mlr_measures_regr.pinball"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      alpha = self$param_set$values$alpha
      probs = attr(prediction$data$quantiles, "probs")
      assert_choice(alpha, probs)

      mlr3measures::pinball(
        truth = prediction$truth,
        response = prediction$data$quantiles[, which(probs == alpha)],
        alpha = alpha
      )
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("regr.pinball", MeasureRegrPinball)
