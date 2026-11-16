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
      scores = as.list(get0(private$.scores_field, learner))
      select = private$.select %??% 1L
      score = if (is.character(select) || length(scores)) scores[[select]]
      if (is.null(score)) {
        return(NA_real_)
      }
      if (!test_number(score, na.ok = TRUE)) {
        error_learner(
          "Learner '%s' must return a single number as validation score '%s' in '$%s'",
          learner$id,
          private$.select %??% names(scores)[1L] %??% "1",
          private$.scores_field
        )
      }
      score
    }
  )
)
