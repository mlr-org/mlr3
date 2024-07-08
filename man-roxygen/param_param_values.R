#' @param param_values (`list()`)\cr
#'   If you want to try many parameter settings for learners, you can pass them through the design
#'   which is optimized to be faster than creating learners for each setting.
#'
#'   A list of lists of named lists, from outer to inner:
#'   1. One list element for each [Learner].
#'   2. One list element for each hyperparameter configuration to try.
#'   3. Named list of hyperparameter settings to set in the Learner, possibly overwriting
#'      already set set hyperparameters in the [Learner].
