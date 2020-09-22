devtools::load_all()
  task = tsk("iris")
  learner = lrn("classif.featureless")
  resampling = rsmp("bootstrap", repeats = 100L)
  store_models = TRUE

self = resample(task, learner, resampling)

  task = assert_task(as_task(task, clone = TRUE))
  learner = assert_learner(as_learner(learner, clone = TRUE))
  resampling = assert_resampling(as_resampling(resampling))
  assert_flag(store_models)
  assert_learnable(task, learner)

  instance = resampling$clone(deep = TRUE)
  if (!instance$is_instantiated) {
    instance = instance$instantiate(task)
  }
  n = instance$iters
  pb = get_progressor(n)

  if (use_future()) {
    lg$debug("Running resample() via future with %i iterations", n)

    res = future.apply::future_lapply(seq_len(n), workhorse,
      task = task, learner = learner, resampling = instance,
      store_models = store_models, lgr_threshold = lg$threshold, pb = pb,
      future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3", future.seed = TRUE
    )
  } else {
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
  }

self =  ResampleResult$new(
  task = task,
  learner = learner,
  states = map(res, "learner_state"),
  resampling = instance,
  iterations = seq_len(n),
  predictions = map(res, "prediction"),
  uhash = NULL
)

private = get_private(self)


if (FALSE) {
  self$aggregate()
  expect_resample_result(self)

}
