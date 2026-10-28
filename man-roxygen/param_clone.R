#' @param clone (`character()`)\cr
#'  Select the input objects to be cloned before proceeding by
#'  providing a set with possible values `"task"`, `"learner"` and
#'  `"resampling"` for [Task], [Learner] and [Resampling], respectively.
#'  Per default, all input objects are cloned.
#'  Objects that are not cloned are used by reference.
#'  For example, `param_values` of a [benchmark()] design are written into the parameter set of the learner.
