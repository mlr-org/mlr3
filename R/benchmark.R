#' @title Benchmark Multiple Learners on Multiple Tasks
#'
#' @description
#' Runs a benchmark on arbitrary combinations of tasks ([Task]), learners ([Learner]), and resampling strategies ([Resampling]), possibly in parallel.
#'
#' @param design ([data.frame()])\cr
#'   Data frame (or [data.table::data.table()]) with three columns: "task", "learner", and "resampling".
#'   Each row defines a resampling by providing a [Task], [Learner] and an instantiated [Resampling] strategy.
#'   The helper function [benchmark_grid()] can assist in generating an exhaustive design (see examples) and
#'   instantiate the [Resampling]s per [Task].
#' @template param_store_models
#' @template param_store_backends
#' @template param_encapsulate
#'
#' @return [BenchmarkResult].
#'
#' @note
#' The fitted models are discarded after the predictions have been scored in order to reduce memory consumption.
#' If you need access to the models for later analysis, set `store_models` to `TRUE`.
#'
#' @template section_parallelization
#' @template section_progress_bars
#' @template section_logging
#'
#' @template seealso_benchmark
#' @export
#' @examples
#' # benchmarking with benchmark_grid()
#' tasks = lapply(c("penguins", "sonar"), tsk)
#' learners = lapply(c("classif.featureless", "classif.rpart"), lrn)
#' resamplings = rsmp("cv", folds = 3)
#'
#' design = benchmark_grid(tasks, learners, resamplings)
#' print(design)
#'
#' set.seed(123)
#' bmr = benchmark(design)
#'
#' ## Data of all resamplings
#' head(as.data.table(bmr))
#'
#' ## Aggregated performance values
#' aggr = bmr$aggregate()
#' print(aggr)
#'
#' ## Extract predictions of first resampling result
#' rr = aggr$resample_result[[1]]
#' as.data.table(rr$prediction())
#'
#' # Benchmarking with a custom design:
#' # - fit classif.featureless on penguins with a 3-fold CV
#' # - fit classif.rpart on sonar using a holdout
#' tasks = list(tsk("penguins"), tsk("sonar"))
#' learners = list(lrn("classif.featureless"), lrn("classif.rpart"))
#' resamplings = list(rsmp("cv", folds = 3), rsmp("holdout"))
#'
#' design = data.table::data.table(
#'   task = tasks,
#'   learner = learners,
#'   resampling = resamplings
#' )
#'
#' ## Instantiate resamplings
#' design$resampling = Map(
#'   function(task, resampling) resampling$clone()$instantiate(task),
#'   task = design$task, resampling = design$resampling
#' )
#'
#' ## Run benchmark
#' bmr = benchmark(design)
#' print(bmr)
#'
#' ## Get the training set of the 2nd iteration of the featureless learner on penguins
#' rr = bmr$aggregate()[learner_id == "classif.featureless"]$resample_result[[1]]
#' rr$resampling$train_set(2)
benchmark = function(design, store_models = FALSE, store_backends = TRUE, encapsulate = NA_character_) {
  assert_data_frame(design, min.rows = 1L)
  assert_names(names(design), permutation.of = c("task", "learner", "resampling"))
  design$task = list(assert_tasks(as_tasks(design$task)))
  design$learner = list(assert_learners(as_learners(design$learner)))
  design$resampling = list(assert_resamplings(as_resamplings(design$resampling), instantiated = TRUE))
  assert_flag(store_models)
  assert_flag(store_backends)

  # check for multiple task types
  assert_same_task_type(c(design$task, design$learner))

  # clone inputs
  setDT(design)
  task = learner = resampling = NULL
  design[, "task" := list(list(task[[1L]]$clone())), by = list(hashes(task))]
  design[, "learner" := list(list(learner[[1L]]$clone())), by = list(hashes(learner))]
  design[, "resampling" := list(list(resampling[[1L]]$clone())), by = list(hashes(resampling))]

  # set encapsulation + fallback
  set_encapsulation(design$learner, encapsulate)

  # expand the design: add rows for each resampling iteration
  grid = pmap_dtr(design, function(task, learner, resampling) {
    # learner = assert_learner(as_learner(learner, clone = TRUE))
    assert_learnable(task, learner)
    data.table(
      task = list(task), learner = list(learner), resampling = list(resampling),
      iteration = seq_len(resampling$iters), uhash = UUIDgenerate()
    )
  })
  n = nrow(grid)

  lg$info("Running benchmark with %i resampling iterations", n)
  pb = if (isNamespaceLoaded("progressr")) {
    # NB: the progress bar needs to be created in this env
    pb = progressr::progressor(steps = n)
  } else {
    NULL
  }

  if (getOption("mlr3.debug", FALSE)) {
    lg$info("Running benchmark() sequentially in debug mode with %i iterations", n)

    res = mapply(workhorse,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iteration,
      MoreArgs = list(store_models = store_models, lgr_threshold = lg$threshold, pb = pb),
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  } else {
    lg$debug("Running benchmark() via future with %i iterations", n)

    res = future.apply::future_mapply(workhorse,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iteration,
      MoreArgs = list(store_models = store_models, lgr_threshold = lg$threshold, pb = pb),
      SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3", future.seed = TRUE,
      future.stdout = NA
    )
  }

  grid = insert_named(grid, list(
    learner_state = map(res, "learner_state"),
    prediction = map(res, "prediction")
  ))

  lg$info("Finished benchmark")

  BenchmarkResult$new(ResultData$new(grid, store_backends = store_backends))
}
