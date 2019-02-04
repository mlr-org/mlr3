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

with_future = function(backend, expr) {
  requireNamespace("future")
  plan = future::plan()
  on.exit(future::plan(plan))
  future::plan(backend)
  force(expr)
}
