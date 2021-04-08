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
#' @param store_models (`logical(1)`)\cr
#'   Store the fitted model in the resulting [BenchmarkResult]?
#'   Set to `TRUE` if you want to further analyse the models or want to
#'   extract information like variable importance.
#' @param store_backends (`logical(1)`)\cr
#'   Keep the [DataBackend] of the [Task] in the [BenchmarkResult]?
#'   Set to `TRUE` if your performance measures require a [Task],
#'   or to analyse results more conveniently.
#'   Set to `FALSE` to reduce the file size and memory footprint
#'   after serialization.
#'   The current default is `TRUE`, but this eventually will be changed
#'   in a future release.
#'
#' @return [BenchmarkResult].
#'
#' @note
#' The fitted models are discarded after the predictions have been scored in order to reduce memory consumption.
#' If you need access to the models for later analysis, set `store_models` to `TRUE`.
#' Optionally, a "retrain" column can be added to the design which contains lists of retrainable [Learner]s.
#' The hyperparameter values of the [Learner] in the "learner" column are applied to the already trained [Learner]s.
#' Then the [Learner]s are retrained instead of trained during the resampling.
#' An empty `list` executes the resampling with `$train()` instead of `$retrain()`.
#' This allows to mix resamplings with `$train()` and `$retrain()`.
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
benchmark = function(design, store_models = FALSE, store_backends = TRUE) {
  assert_data_frame(design, min.rows = 1L)
  assert_names(names(design), must.include = c("task", "learner", "resampling"), subset.of = c("task", "learner", "resampling", "retrain"))
  design$task = list(assert_tasks(as_tasks(design$task)))
  design$learner = list(assert_learners(as_learners(design$learner)))
  design$resampling = list(assert_resamplings(as_resamplings(design$resampling), instantiated = TRUE))
  assert_flag(store_models)
  if (is.null(design$retrain)) set(design, j = "retrain", value = list(list()))

  # check for multiple task types
  task_types = unique(map_chr(design$task, "task_type"))
  if (length(task_types) > 1L) {
    stopf("Multiple task types detected: %s", str_collapse(task_types))
  }

  # clone inputs
  setDT(design)
  task = learner = resampling = NULL
  design[, "task" := list(list(task[[1L]]$clone())), by = list(hashes(task))]
  design[, "learner" := list(list(learner[[1L]]$clone())), by = list(hashes(learner))]
  design[, "resampling" := list(list(resampling[[1L]]$clone())), by = list(hashes(resampling))]

  # clone retrain learners and set hyperparameter values
  rls = pmap(list(design$learner, design$retrain), function(learner, rls) map(rls, function(rl) {
    rlc = rl$clone()
    rlc$param_set$values = insert_named(rlc$param_set$values, learner$param_set$values)
    rlc
  }))
  set(design, j = "retrain", value = rls)

  # expand the design: add rows for each resampling iteration
  grid = pmap_dtr(design, function(task, learner, resampling, retrain) {
    # learner = assert_learner(as_learner(learner, clone = TRUE))
    assert_learnable(task, learner)
    learner = if (length(retrain) > 0) retrain else list(learner)
    mode = if (length(retrain) > 0) "retrain" else "train"

    data.table(
      task = list(task), learner = learner, resampling = list(resampling),
      iteration = seq_len(resampling$iters), mode = mode, uhash = UUIDgenerate()
    )
  })
  n = nrow(grid)

  lg$info("Running benchmark with %i resampling iterations", n)
  pb = get_progressor(n)

  if (getOption("mlr3.debug", FALSE)) {
    lg$info("Running benchmark() sequentially in debug mode with %i iterations", n)

    res = mapply(workhorse,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iteration, mode = grid$mode,
      MoreArgs = list(store_models = store_models, lgr_threshold = lg$threshold, pb = pb),
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  } else {
    lg$debug("Running benchmark() via future with %i iterations", n)

    res = future.apply::future_mapply(workhorse,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iteration, mode = grid$mode,
      MoreArgs = list(store_models = store_models, lgr_threshold = lg$threshold, pb = pb),
      SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3", future.seed = TRUE
    )
  }

  grid = insert_named(grid, list(
    learner_state = map(res, "learner_state"),
    prediction = map(res, "prediction")
  ))
  grid$mode = NULL

  lg$info("Finished benchmark")

  BenchmarkResult$new(ResultData$new(grid, store_backends = store_backends))
}
