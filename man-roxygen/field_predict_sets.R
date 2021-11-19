#' @field predict_sets (`character()`)\cr
#'   During [resample()]/[benchmark()], a [Learner] can predict on multiple sets.
#'   Per default, a learner only predicts observations in the test set (`predict_sets == "test"`).
#'   To change this behavior, set `predict_sets` to a non-empty subset of `{"train", "test", "validation"}`.
#'   Each set yields a separate [Prediction] object.
#'   Those be combined via getters in [ResampleResult]/[BenchmarkResult], or [Measure]s can be altered
#'   to operate on specific subsets of the calculated prediction sets.
