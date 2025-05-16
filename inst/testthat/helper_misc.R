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
  oplan = force(future::plan(backend, ...))
  on.exit(future::plan(oplan), add = TRUE)
  force(expr)
}

private = function(x) {
  x[[".__enclos_env__"]][["private"]]
}


iris_weights_learner = TaskClassif$new("iris_weights_learner", as_data_backend(cbind(datasets::iris, data.frame(w = rep(c(1, 10, 100), each = 50)))), target = "Species")
iris_weights_learner$set_col_roles("w", "weights_learner")

iris_weights_measure = TaskClassif$new("iris_weights_measure", as_data_backend(cbind(datasets::iris, data.frame(w = rep(c(1, 10, 100), each = 50)))), target = "Species")
iris_weights_measure$set_col_roles("w", "weights_measure")

cars_weights_learner = TaskRegr$new("cars_weights_learner", as_data_backend(cbind(datasets::cars, data.frame(w = rep(c(1, 10), each = 25)))), target = "dist")
cars_weights_learner$set_col_roles("w", "weights_learner")

cars_weights_measure = TaskRegr$new("cars_weights_measure", as_data_backend(cbind(datasets::cars, data.frame(w = rep(c(1, 10), each = 25)))), target = "dist")
cars_weights_measure$set_col_roles("w", "weights_measure")

