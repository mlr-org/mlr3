#' @title Measure Best Validation Score
#'
#' @name mlr_measures_best_valid_score
#' @include MeasureValidScore.R
#'
#' @description
#' Returns the selected best internal validation score of the [Learner].
#' This is only available for learners that have both the `"validation"` and the `"internal_tuning"` property,
#' because tracking a best iteration only makes sense for learners that iterate.
#' Returns `NA` for unsupported learners, when no validation was done, or when the selected id was not found.
#' The `id` of this measure is set to the value of `select` if provided.
#'
#' Note that not every such learner tracks the best validation score:
#' this requires the learner to implement the private method `$.extract_best_valid_scores()`
#' (see [`Learner`], section *Implementing Validation*).
#' For learners that do not, this measure returns `NA`.
#' Whether a learner has the `"internal_tuning"` property is not checked, only whether it implements that method,
#' so this measure relies on learners following that convention.
#'
#' While [`msr("internal_valid_score")`][mlr_measures_internal_valid_score] reports the validation score of the
#' *final* model, this measure reports the *best* validation score observed during training.
#' These two can differ: the internally tuned values (see `$internal_tuned_values`) usually correspond to the
#' iteration with the best validation score, whereas the final model is the one after the last iteration.
#'
#' Some learners automatically use the best found model for prediction instead of the one from the last iteration.
#' For those the two measures report the same value.
#' Whether a learner does this is always documented with the learner itself.
#'
#' @templateVar id best_valid_score
#' @template measure
#'
#' @template seealso_measure
#' @export
#' @examples
#' rr = resample(tsk("iris"), lrn("classif.debug", validate = 0.3), rsmp("holdout"))
#' rr$score(msr("best_valid_score", select = "acc"))
MeasureBestValidScore = R6Class(
  "MeasureBestValidScore",
  inherit = MeasureValidScore,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param select  (`character(1)`)\cr
    #'   Which of the best validation scores to select.
    #'   Which scores are available depends on the learner and its configuration.
    #'   By default, the first score is chosen.
    #' @param minimize (`logical(1)`)\cr
    #'   Whether smaller values are better.
    #'   Must be set to use for tuning.
    initialize = function(select = NULL, minimize = NA) {
      super$initialize(
        scores_field = "best_valid_scores",
        id = "best_valid_score",
        label = "Best Validation Score",
        man = "mlr3::mlr_measures_best_valid_score",
        select = select,
        minimize = minimize
      )
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("best_valid_score", MeasureBestValidScore)
