# determines if execution via future will be running locally or remotely
use_future = function() {
  if (!isNamespaceLoaded("future"))
    return(FALSE)
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    warningf("Package future.apply could not be loaded. Parallelization disabled.")
    return(FALSE)
  }
  !inherits(future::plan(), "uniprocess")
}

# determines if execution will be running in the current session or in a remote session
runs_remotely = function(ctrl, stages = c("train", "predict")) {
  return(set_names(rep(TRUE, length(stages)), stages))
  set_names(ctrl[sprintf("encapsulate_%s", stages)] == "callr", stages)
}

# copies model from a list of learners in `src` to a list of learners in `dst`
# all learners in `dst` will be cloned
reassemble_learners = function(src, dst) {
  Map(function(src, dst) {
    dst = dst$clone(deep = TRUE)
    dst$model = src$model
    dst$param_set$values = src$param_set$values
    dst
  }, src = src, dst = dst)
}
