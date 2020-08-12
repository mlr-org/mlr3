# determines if execution via future will be running locally or remotely
use_future = function() {
  isNamespaceLoaded("future") && !inherits(future::plan(), "uniprocess")
}

get_rng_state = function() {
  list(seed = get_seed(), kind = RNGkind())
}

restore_rng_state = function(prev) {
  do.call(RNGkind, as.list(prev$kind))
  assign(".Random.seed", value = prev$seed, envir = .GlobalEnv)
}

init_future_seeding = function(n) {
  RNGkind("L'Ecuyer-CMRG")
  getFromNamespace("make_rng_seeds", asNamespace("future.apply"))(n, TRUE)
}

get_progressor = function(n, label = NA_character_) {
  if (!isNamespaceLoaded("progressr")) {
    return(NULL)
  }

  progressr::progressor(steps = n, label = label)
}
