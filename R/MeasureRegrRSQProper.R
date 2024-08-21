#' @title Proper R-Squared
#'
#' @name mlr_measures_regr.rsq_proper
#' @include Measure.R
#'
#' @description
#' Calculates the \eqn{R^2} for a regression problem.
#' In contrast to [mlr_measures_regr.rsq], the mean of the true values is
#' calculated on the training set instead of the test set.
#'
#' @templateVar id rsq_proper
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureRegrRSQ = R6Class("MeasureRSQ",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(test_mean = TRUE) {
      private$.test_mean = assert_flag(test_mean)

      super$initialize(
        id = "rsq",
        task_type = "regr",
        properties = if (!private$.test_mean) c("requires_task", "requires_train_set") else character(0),
        predict_type = "response",
        minimize = FALSE,
        man = "mlr3::mlr_measures_regr.rsq"
      )
    }
  ),

  private = list(
    .test_mean = NULL,

    .score = function(prediction, truth, task = NULL, train_set = NULL, ...) {

      den = if (private$.test_mean) {
        v = var(truth)
        if (v < sqrt(.Machine$double.eps)) {
          return(na_value)
        }
        v * (length(truth) - 1L)
      } else {
        mu = mean(task$truth(train_set))
        sum((truth - mu)^2)
      }

      1 - sum((truth - prediction$response)^2) / den
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("regr.rsq", MeasureRegrRSQ)
