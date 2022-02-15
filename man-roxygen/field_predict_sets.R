#' @field predict_sets (`character()`)\cr
#'   During [resample()]/[benchmark()], a [Learner] can predict on multiple sets.
#'   Per default, a learner only predicts observations in the validation set (`predict_sets == "validation"`).
#'   To change this behavior, set `predict_sets` to a non-empty subset of `{"train", "validation", "holdout"}`.
#'   Each set yields a separate [Prediction] object.
#'   Those can be combined via getters in [ResampleResult]/[BenchmarkResult], or [Measure]s can be configured
#'   to operate on specific subsets of the calculated prediction sets.
#'
#'   The set `"test"` is an deprecated alias name for the validation set.
