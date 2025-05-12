#' @param ties_method (`character(1)`)\cr
#'   Method to handle ties in probabilities when selecting a class label.
#'   Must be one of `"random"`, `"first"` or `"last"` (corresponding to the same options in [max.col()]).
#'   * `"random"`: Randomly select one of the tied class labels (default).
#'   * `"first"`: Select the first class label among tied values.
#'   * `"last"`: Select the last class label among tied values.
