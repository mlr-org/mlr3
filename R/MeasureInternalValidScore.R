#' @title Measure Internal Validation Score
#'
#' @name mlr_measures_internal_valid_score
#' @include MeasureValidScore.R
#'
#' @description
#' Returns the selected internal validation score of the [Learner] for learners with property `"validation"`.
#' Returns `NA` for unsupported learners, when no validation was done, or when the selected id was not found.
#' The `id` of this measure is set to the value of `select` if provided.
#'
#' This is the validation score of the *final* model.
#' To obtain the *best* validation score that was observed during training, use
#' [`msr("best_valid_score")`][mlr_measures_best_valid_score].
#'
#' @templateVar id internal_valid_score
#' @template measure
#'
#' @template seealso_measure
#' @export
#' @examples
#' rr = resample(tsk("iris"), lrn("classif.debug", validate = 0.3), rsmp("holdout"))
#' rr$score(msr("internal_valid_score", select = "acc"))
MeasureInternalValidScore = R6Class(
  "MeasureInternalValidScore",
  inherit = MeasureValidScore,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param select  (`character(1)`)\cr
    #'   Which of the internal validation scores to select.
    #'   Which scores are available depends on the learner and its configuration.
    #'   By default, the first score is chosen.
    #' @param minimize (`logical(1)`)\cr
    #'   Whether smaller values are better.
    #'   Must be set to use for tuning.
    initialize = function(select = NULL, minimize = NA) {
      super$initialize(
        scores_field = "internal_valid_scores",
        id = "internal_valid_score",
        label = "Internal Validation Score",
        man = "mlr3::mlr_measures_internal_valid_score",
        select = select,
        minimize = minimize
      )
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("internal_valid_score", MeasureInternalValidScore)
