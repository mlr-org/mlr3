#' @param unmarshal [`Learner`]\cr
#'   Whether to unmarshal learners that were sent back to the main process in marshaled form.
#'   Setting this to `TRUE` does not guarantee that all learners that require marshaling are marshaled.
#'   To achieve this, call `$marshal()` on the result object.
