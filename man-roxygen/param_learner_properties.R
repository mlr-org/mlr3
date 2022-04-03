#' @param properties (`character()`)\cr
#'   Set of properties of the [Learner].
#'   Must be a subset of [`mlr_reflections$learner_properties`][mlr_reflections].
#'   The following properties are currently standardized and understood by learners in \CRANpkg{mlr3}:
#'   * `"missings"`: The learner can handle missing values in the data.
#'   * `"weights"`: The learner supports observation weights.
#'   * `"importance"`: The learner supports extraction of importance scores, i.e. comes with an `$importance()` extractor function (see section on optional extractors in [Learner]).
#'   * `"selected_features"`: The learner supports extraction of the set of selected features, i.e. comes with a `$selected_features()` extractor function (see section on optional extractors in [Learner]).
#'   * `"oob_error"`: The learner supports extraction of estimated out of bag error, i.e. comes with a `oob_error()` extractor function (see section on optional extractors in [Learner]).
#'   * `validation`: The learner supports the use of validation data (e.g. for early stopping).
#'   For learners with this property the argument `valid_ids` can be set when calling the `$train`
#'   method of the learner. During a `resample()` call the learner also has access to the validation
#'   ids (in earlier versions this set was named test set).
