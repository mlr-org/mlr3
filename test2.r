devtools::load_all()
tasks = mlr_tasks$mget(c("iris", "sonar"))
tasks = c(tasks, list(tsk("iris")$select("Sepal.Length")))
learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
resamplings = rsmp("cv", folds = 3)
design = benchmark_grid(tasks, learners, resamplings)
store_models = TRUE

  assert_data_frame(design, min.rows = 1L)
  assert_names(names(design), permutation.of = c("task", "learner", "resampling"))
  design$task = list(assert_tasks(as_tasks(design$task)))
  design$resampling = list(assert_resamplings(as_resamplings(design$resampling), instantiated = TRUE))
  assert_flag(store_models)

  # check for multiple task types
  task_types = unique(map_chr(design$task, "task_type"))
  if (length(task_types) > 1L) {
    stopf("Multiple task types detected: %s", str_collapse(task_types))
  }

  # clone inputs
  setDT(design)
  task = resampling = NULL
  design[, "task" := list(list(task[[1L]]$clone())), by = list(hashes(task))]
  design[, "resampling" := list(list(resampling[[1L]]$clone())), by = list(hashes(resampling))]

  # expand the design: add rows for each resampling iteration
  grid = pmap_dtr(design, function(task, learner, resampling) {
    # we do not need to clone the learner here because we clone it before training
    learner = assert_learner(as_learner(learner))
    assert_learnable(task, learner)
    data.table(
      task = list(task), learner = list(learner), resampling = list(resampling),
      iteration = seq_len(resampling$iters), uhash = UUIDgenerate()
    )
  })
  n = nrow(grid)

  lg$info("Benchmark with %i resampling iterations", n)
  pb = get_progressor(n)

  if (use_future()) {
    lg$debug("Running benchmark() asynchronously with %i iterations", n)

    res = future.apply::future_mapply(workhorse,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iteration,
      MoreArgs = list(store_models = store_models, lgr_threshold = lg$threshold, pb = pb),
      SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3", future.seed = TRUE
    )
  } else {
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
  }

  grid = insert_named(grid, list(
      state = map(res, "learner_state"),
      prediction = map(res, "prediction")
  ))

data = copy(grid)

self = bmr = BenchmarkResult$new(data)
private = get_private(self)
# self$tasks
# self$learners
# self$resamplings
self$aggregate()
