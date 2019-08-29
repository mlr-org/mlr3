#' @title Benchmark Multiple Learners on Multiple Tasks
#'
#' @description
#' Runs a benchmark on arbitrary combinations of learners, tasks, and resampling strategies (possibly in parallel).
#' Resamplings which are not already instantiated will be instantiated automatically.
#' However, these auto-instantiated resamplings will not be synchronized per task, i.e. different learners will
#' work on different splits of the same task.
#'
#' To generate exhaustive designs and automatically instantiate resampling strategies per task, use [benchmark_grid()].
#'
#' @param design :: [data.frame()]\cr
#'   Data frame (or [data.table::data.table()]) with three columns: "task", "learner", and "resampling".
#'   Each row defines a resampling by providing a [Task], [Learner] and a [Resampling] strategy.
#'   All resamplings must be properly instantiated.
#'   The helper function [benchmark_grid()] can assist in generating an exhaustive design (see examples) and
#'   instantiate the [Resampling]s per [Task].
#' @param store_models :: `logical(1)`\cr
#'   Keep the fitted model after the test set has been predicted?
#'   Set to `TRUE` if you want to further analyse the models or want to
#'   extract information like variable importance.
#' @return [BenchmarkResult].
#'
#' @note
#' The fitted models are discarded after the predictions have been scored in order to reduce memory consumption.
#' If you need access to the models for later analysis, set `store_models` to `TRUE`.
#'
#' @template section_parallelization
#' @template section_logging
#'
#' @export
#' @examples
#' # benchmarking with benchmark_grid()
#' tasks = lapply(c("iris", "sonar"), tsk)
#' learners = lapply(c("classif.featureless", "classif.rpart"), lrn)
#' resamplings = rsmp("cv", folds = 3)
#'
#' design = benchmark_grid(tasks, learners, resamplings)
#' print(design)
#'
#' set.seed(123)
#' bmr = benchmark(design)
#'
#' ## data of all resamplings
#' head(as.data.table(bmr))
#'
#' ## aggregated performance values
#' aggr = bmr$aggregate()
#' print(aggr)
#'
#' ## Extract predictions of first resampling result
#' rr = aggr$resample_result[[1]]
#' as.data.table(rr$prediction())
#'
#' # benchmarking with a custom design:
#' # - fit classif.featureless on iris with a 3-fold CV
#' # - fit classif.rpart on sonar using a holdout
#' tasks = list(tsk("iris"), tsk("sonar"))
#' learners = list(lrn("classif.featureless"), lrn("classif.rpart"))
#' resamplings = list(rsmp("cv", folds = 3), rsmp("holdout"))
#'
#' design = data.table::data.table(
#'   task = tasks,
#'   learner = learners,
#'   resampling = resamplings
#' )
#'
#' ## instantiate resamplings
#' design$resampling = Map(
#'   function(task, resampling) resampling$clone()$instantiate(task),
#'   task = design$task, resampling = design$resampling
#' )
#'
#' ## run benchmark
#' bmr = benchmark(design)
#' print(bmr)
#'
#' ## get the training set of the 2nd iteration of the featureless learner on iris
#' rr = bmr$aggregate()[learner_id == "classif.featureless"]$resample_result[[1]]
#' rr$resampling$train_set(2)
benchmark = function(design, store_models = FALSE) {

  assert_data_frame(design, min.rows = 1L)
  assert_names(names(design), permutation.of = c("task", "learner", "resampling"))
  design$task = list(assert_tasks(design$task))
  design$resampling = list(assert_resamplings(design$resampling, instantiated = TRUE))
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
    assert_learner(learner, task = task, properties = task$properties)
    hash = hash_resample_result(task, learner, resampling)
    data.table(
      task = list(task), learner = list(learner), resampling = list(resampling),
      iteration = seq_len(resampling$iters), hash = hash)
  })

  lg$info("Benchmark with %i resampling iterations", nrow(grid))

  if (use_future()) {
    lg$debug("Running benchmark() via future")

    res = future.apply::future_mapply(workhorse,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iteration, MoreArgs = list(store_models = store_models, lgr_threshold = lg$threshold),
      SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3"
    )
  } else {
    lg$debug("Running benchmark() sequentially")

    res = mapply(workhorse,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iteration, MoreArgs = list(store_models = store_models), SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  }

  res = rbindlist(Map(reassemble, result = res, learner = grid$learner), use.names = TRUE)
  res = insert_named(grid, res)

  lg$info("Finished benchmark")
  BenchmarkResult$new(res)
}
