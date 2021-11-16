#' @param properties (`character()`)\cr
#'   Properties of the measure.
#'   Must be a subset of [mlr_reflections$measure_properties][mlr_reflections].
#'   Supported by `mlr3`:
#'   * `"requires_task"` (requires the complete [Task]),
#'   * `"requires_learner"` (requires the trained [Learner]),
#'   * `"requires_model"` (requires the trained [Learner], including the fitted
#'   model),
#'   * `"requires_train_set"` (requires the training indices from the [Resampling]), and
#'   * `"na_score"` (the measure is expected to occasionally return `NA` or `NaN`).
