#' @title Inner Validation Score
#'
#' @name mlr_measures_inner_valid_score
#' @include Measure.R
#'
#' @description
#' Returns the selected inner validation score of the [Learner] for learners that support it
#' (learners with property `"validation"`).
#' Returns `NA` for unsupported learners, when no validation was done, or when the selected id was not found.
#'
#' @section Parameters:
#' * `select` : (`character(1)`)\cr
#'   Which of the inner validation scores to select.
#'
#' @templateVar id inner_valid_score
#' @template measure
#'
#' @template seealso_measure
#' @export
#' @examples
#' rr = resample(tsk("iris"), lrn("classif.debug", validate = 0.3), rsmp("holdout"))
#' rr$score(msr("inner_valid_score", select = "acc"))
MeasureInnerValidScore = R6Class("MeasureInnerValidScore",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "inner_valid_score",
        task_type = NA_character_,
        properties = c("na_score", "requires_learner"),
        predict_type = NA_character_,
        param_set = ps(
          select = p_uty(tags = "required", custom_check = check_string)
        ),
        range = c(-Inf, Inf),
        minimize = NA,
        label = "Inner Validation Score",
        man = "mlr3::mlr_measures_inner_valid_score"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      get0("inner_valid_scores", learner)[[self$param_set$get_values()$select]] %??% NA_real_
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("inner_valid_score", function() MeasureInnerValidScore$new())
