hash = function(x) {
  UseMethod("hash")
}

hash.list = function(x) {
  digest::digest(c(x$task$hash, x$learner$hash, x$resampling$hash), algo = "xxhash64")
}

hash.Experiment = function(x) {
  hash.list(x$data)
}

hash.Task = function(x) {
  digest::digest(list(x$id, x$row_roles, x$col_roles), algo = "xxhash64")
}

hash.Learner = function(x) {
  digest::digest(list(x$id, x$par_vals), algo = "xxhash64")
}

hash.Resampling = function(x) {
  digest::digest(list(x$id, x$par_vals, x$instance), algo = "xxhash64")
}
