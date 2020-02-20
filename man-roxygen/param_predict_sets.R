#' @param predict_sets (`character()`)\cr
#'   Prediction sets to operate on, used in `aggregate()` to extract the matching `predict_sets` from the [ResampleResult].
#'   Multiple predict sets are calculated by the respective [Learner] during [resample()]/[benchmark()].
#'   Must be a non-empty subset of `{"train", "test"}`.
#'   If multiple sets are provided, these are first combined to a single prediction object.
#'   Default is `"test"`.
