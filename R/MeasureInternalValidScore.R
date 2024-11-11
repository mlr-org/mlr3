#' @title Measure Internal Validation Score
#'
#' @name mlr_measures_internal_valid_score
#' @include Measure.R
#'
#' @description
#' Returns the selected internal validation score of the [Learner] for learners property `"validation"`.
#' Returns `NA` for unsupported learners, when no validation was done, or when the selected id was not found.
#' The `id` of this measure is set to the value of `select` if provided.
#'
#' @templateVar id internal_valid_score
#' @template measure
#'
#' @template seealso_measure
#' @export
#' @examples
#' rr = resample(tsk("iris"), lrn("classif.debug", validate = 0.3), rsmp("holdout"))
#' rr$score(msr("internal_valid_score", select = "acc"))
MeasureInternalValidScore = R6Class("MeasureInternalValidScore",
  inherit = Measure,
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
      private$.select = assert_string(select, null.ok = TRUE)
      super$initialize(
        id = select %??% "internal_valid_score",
        task_type = NA_character_,
        properties = c("na_score", "requires_model", "requires_learner", "requires_no_prediction"),
        predict_sets = NULL,
        predict_type = NA_character_,
        range = c(-Inf, Inf),
        minimize = assert_flag(minimize, na.ok = TRUE),
        label = "Internal Validation Score",
        man = "mlr3::mlr_measures_internal_valid_score"
      )
    }
  ),

  private = list(
    .select = NULL,
    .score = function(prediction, learner, ...) {
      x = get0("internal_valid_scores", learner)
      x[[private$.select %??% 1]] %??% NA_real_
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("internal_valid_score", MeasureInternalValidScore)
