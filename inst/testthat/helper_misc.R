with_seed = function(seed, expr) {
  old_seed = get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  if (is.null(old_seed)) {
    runif(1L)
    old_seed = get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  }

  on.exit(assign(".Random.seed", old_seed, globalenv()), add = TRUE)
  set.seed(seed)
  force(expr)
}

with_future = function(backend, expr, ...) {
  requireNamespace("future")
  oplan = future::plan(backend, ...)
  on.exit(future::plan(oplan))
  force(expr)
}

private = function(x) {
  x[[".__enclos_env__"]][["private"]]
}
