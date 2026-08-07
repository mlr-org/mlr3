#' @include Measure.R
#'
#' Base class for measures that read a named list of validation scores from a field of the [Learner].
#' Not exported; see [MeasureInternalValidScore] and [MeasureBestValidScore] for the concrete measures.
#' @noRd
MeasureValidScore = R6Class(
  "MeasureValidScore",
  inherit = Measure,
  public = list(
    initialize = function(scores_field, id, label, man, select = NULL, minimize = NA) {
      private$.scores_field = assert_string(scores_field)
      private$.select = assert_string(select, null.ok = TRUE)
      super$initialize(
        id = select %??% id,
        task_type = NA_character_,
        properties = c("na_score", "requires_learner", "requires_no_prediction"),
        predict_sets = NULL,
        predict_type = NA_character_,
        range = c(-Inf, Inf),
        minimize = assert_flag(minimize, na.ok = TRUE),
        label = label,
        man = man
      )
    }
  ),

  private = list(
    .scores_field = NULL,
    .select = NULL,
    .score = function(prediction, learner, ...) {
      x = get0(private$.scores_field, learner)
      x[[private$.select %??% 1]] %??% NA_real_
    }
  )
)
