#' @param unmarshal [`Learner`]\cr
#'   Whether to unmarshal learners that were marshaled during the execution.
#'   Setting this to `FALSE` does not guarantee that the learners are marshaled.
#'   For example, with sequential execution and no encapsulation, marshaling is not necessary.
#'   If you want all learners in marshaled form, you need to call `$marshal()` on the result object.
