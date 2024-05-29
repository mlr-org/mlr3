#' @title Measure Internal Validation Score
#'
#' @name mlr_measures_internal_valid_score
#' @include Measure.R
#'
#' @description
#' Returns the selected internal validation score of the [Learner] for learners property `"validation"`.
#' Returns `NA` for unsupported learners, when no validation was done, or when the selected id was not found.
#'
#' @section Parameters:
#' * `select` : (`character(1)`)\cr
#'   Which of the internal validation scores to select.
#'   Which scores are available depends on the learner.
#'   By default, the first score is chosen.
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
    initialize = function() {
      super$initialize(
        id = "internal_valid_score",
        task_type = NA_character_,
        properties = c("na_score", "requires_learner"),
        predict_type = NA_character_,
        param_set = ps(
          select = p_uty(custom_check = check_string)
        ),
        range = c(-Inf, Inf),
        minimize = NA,
        label = "Internal Validation Score",
        man = "mlr3::mlr_measures_internal_valid_score"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      x = get0("internal_valid_scores", learner)
      x[[self$param_set$get_values()$select %??% 1]] %??% NA_real_
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("internal_valid_score", function() MeasureInternalValidScore$new())
