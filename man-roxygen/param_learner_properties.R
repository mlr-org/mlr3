#' @param properties (`character()`)\cr
#'   Set of properties of the [Learner].
#'   Must be a subset of [`mlr_reflections$learner_properties`][mlr_reflections].
#'   The following properties are currently standardized and understood by learners in \CRANpkg{mlr3}:
#'   * `"missings"`: The learner can handle missing values in the data.
#'   * `"weights"`: The learner supports observation weights.
#'   * `"importance"`: The learner supports extraction of importance scores, i.e. comes with a `importance()` extractor function (see section on optional extractors).
#'   * `"selected_features"`: The learner supports extraction of the set of selected features, i.e. comes with a `selected_features()` extractor function (see section on optional extractors).
#'   * `"oob_error"`: The learner supports extraction of estimated out of bag error, i.e. comes with a `oob_error()` extractor function (see section on optional extractors).
