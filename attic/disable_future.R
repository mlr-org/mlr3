# from resample()
    lg$debug("Running resample() sequentially with %i iterations", n)

    prev = get_rng_state()
    on.exit(restore_rng_state(prev))
    seeds = init_future_seeding(n)

    res = vector("list", n)
    for (i in seq_len(n)) {
      assign(".Random.seed", value = seeds[[i]], envir = .GlobalEnv)
      res[[i]] = workhorse(i, task = task, learner = learner, resampling = instance,
        store_models = store_models, lgr_threshold = lg$threshold, pb = pb)
    }

# from benchmark()
    lg$debug("Running benchmark() sequentially with %i iterations", n)

    prev = get_rng_state()
    on.exit(restore_rng_state(prev))
    seeds = init_future_seeding(n)

    res = vector("list", n)
    for (i in seq_len(n)) {
      assign(".Random.seed", value = seeds[[i]], envir = .GlobalEnv)

      row = unlist(grid[i], recursive = TRUE)
      res[[i]] = workhorse(row$iteration, task = row$task, learner = row$learner,
        resampling = row$resampling, lgr_threshold = lg$threshold,
        store_models = store_models, pb = pb)
    }

# helpers
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
