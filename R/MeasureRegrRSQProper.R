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
MeasureRegrRSQProper = R6Class("MeasureRSQProper",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "rsq_proper",
        task_type = "regr",
        properties = c("requires_task", "requires_train_set"),
        predict_type = "response",
        minimize = FALSE,
        man = "mlr3::mlr_measures_regr.rsq_proper"
      )
    }
  ),

  private = list(
    .score = function(prediction, truth, task, train_set, ...) {
      mu = mean(task$truth(train_set))
      1 - sum((truth - prediction$response)^2) / sum((truth - mu)^2)

    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("regr.rsq_proper", MeasureRegrRSQProper)
