#' @title Measure Best Validation Score
#'
#' @name mlr_measures_best_valid_score
#' @include Measure.R
#'
#' @description
#' Returns the selected best internal validation score of the [Learner] for learners with property `"validation"`.
#' Returns `NA` for unsupported learners, when no validation was done, or when the selected id was not found.
#' The `id` of this measure is set to the value of `select` if provided.
#'
#' Note that not every learner with the `"validation"` property tracks the best validation score:
#' this requires the learner's `$.extract_internal_valid_scores()` method to support the `which` argument
#' (see [`Learner`], section *Implementing Validation*).
#' For learners that do not, this measure returns `NA`.
#'
#' While [`msr("internal_valid_score")`][mlr_measures_internal_valid_score] reports the validation score of the
#' *final* model, this measure reports the *best* validation score observed during training.
#' For learners that internally tune a hyperparameter such as the number of boosting rounds or epochs, these two
#' can differ: the internally tuned values (see `$internal_tuned_values`) usually correspond to the iteration with
#' the best validation score, whereas the final model is the one after the last iteration.
#' Which of the two is the appropriate tuning measure depends on the learner:
#' if the model that is used for prediction is the one from the best iteration, use this measure,
#' otherwise use [`msr("internal_valid_score")`][mlr_measures_internal_valid_score].
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
  inherit = Measure,
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
      private$.select = assert_string(select, null.ok = TRUE)
      super$initialize(
        id = select %??% "best_valid_score",
        task_type = NA_character_,
        properties = c("na_score", "requires_learner", "requires_no_prediction"),
        predict_sets = NULL,
        predict_type = NA_character_,
        range = c(-Inf, Inf),
        minimize = assert_flag(minimize, na.ok = TRUE),
        label = "Best Validation Score",
        man = "mlr3::mlr_measures_best_valid_score"
      )
    }
  ),

  private = list(
    .select = NULL,
    .score = function(prediction, learner, ...) {
      x = get0("best_valid_scores", learner)
      x[[private$.select %??% 1]] %??% NA_real_
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("best_valid_score", MeasureBestValidScore)
