future_remote = function() {
  requireNamespace("future", quietly = TRUE) &&
  requireNamespace("future.apply", quietly = TRUE) &&
  !inherits(future::plan(), "uniprocess")
}
