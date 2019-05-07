use_future = function() {
  requireNamespace("future", quietly = TRUE) &&
  requireNamespace("future.apply", quietly = TRUE) &&
  !inherits(future::plan(), "uniprocess")
}
