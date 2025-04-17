#' @title R-Squared
#'
#' @name mlr_measures_regr.rsq
#' @include Measure.R
#'
#' @description
#' Measure to compare true observed response with predicted response in regression tasks.
#'
#' @details
#' R Squared is defined as \deqn{
#'   1 - \frac{\sum_{i=1}^n w_i \left( t_i - r_i \right)^2}{\sum_{i=1}^n w_i \left( t_i - \bar{t} \right)^2},
#' }{
#'   1 - sum(w * (t - r)^2) / sum(w * (t - mean(t))^2),
#' }
#' where \eqn{\bar{t} = \frac{1}{n} \sum_{i=1}^n t_i} and \eqn{w_i} are weights.
#'
#' Also known as coefficient of determination or explained variation.
#' It compares the squared error of the predictions relative to a naive model predicting the mean.
#'
#' Note that weights are used to scale the squared error of individual predictions (both in the numerator and in the denominator),
#' but the "plug in" value \eqn{\bar{t}} is computed without weights.
#'
#' This measure is undefined for constant \eqn{t}.
#'
#' @param pred_set_mean `logical(1)`\cr
#' If `TRUE`, the mean of the true values is calculated on the prediction set.
#' If `FALSE`, the mean of the true values is calculated on the training set.
#'
#' @templateVar id regr.rsq
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureRegrRSQ = R6Class("MeasureRSQ",
  inherit = MeasureRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(pred_set_mean = TRUE) {
      # this is not included in the paramset as this flag influences properties of the learner
      # so this flag should not be "dynamic state"
      private$.pred_set_mean = assert_flag(pred_set_mean)

      super$initialize(
        id = "regr.rsq",
        properties = c(if (!private$.pred_set_mean) c("requires_task", "requires_train_set"), "weights"),
        predict_type = "response",
        minimize = FALSE,
        range = c(-Inf, 1),
        man = "mlr3::mlr_measures_regr.rsq"
      )
    }
  ),

  private = list(
    # this is not included in the paramset as this flag influences properties of the learner
    # so this flag should not be "dynamic state"
    .pred_set_mean = NULL,

    .score = function(prediction, task = NULL, train_set = NULL, weights = NULL, ...) {
      if (is.null(weights)) {
        mu = if (private$.pred_set_mean) mean(prediction$truth) else mean(task$truth(train_set))
        1 - sum((prediction$truth - prediction$response)^2) / sum((prediction$truth - mu)^2)
      } else {
        # Don't use weighted mean here, since resampling weights only concern weighting of loss of individual predictions.
        # One could argue that we should use weights when .pred_set.mean is TRUE, or we could use weights_learner from the task,
        # but currently we decided against it.
        mu = if (private$.pred_set_mean) {
          mean(prediction$truth)
        } else {
          mean(task$truth(train_set))
        }
        1 - sum(weights * (prediction$truth - prediction$response)^2) / sum(weights * (prediction$truth - mu)^2)
      }
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("regr.rsq", MeasureRegrRSQ)
