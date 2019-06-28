#' @title Benchmark Multiple Learners on Multiple Tasks
#'
#' @description
#' Runs a benchmark on arbitrary combinations of learners, tasks, and resampling strategies (possibly in parallel).
#' Resamplings which are not already instantiated will be instantiated automatically.
#' However, these auto-instantiated resamplings will not be synchronized per task, i.e. different learners will
#' work on different splits of the same task.
#'
#' To generate exhaustive designs and automatically instantiate resampling strategies per task, use [expand_grid()].
#'
#' @param design :: [data.frame()]\cr
#'   Data frame (or [data.table()]) with three columns: "task", "learner", and "resampling".
#'   Each row defines a set of resampled experiments by providing a [Task], [Learner] and [Resampling] strategy.
#'   All resamplings must be properly instantiated.
#'   The helper function [expand_grid()] can assist in generating an exhaustive design (see examples) and
#'   instantiate the [Resampling]s per [Task].
#' @param ctrl :: (named `list()`)\cr
#'   Object to control experiment execution. See [mlr_control()] for details.
#'   Note that per default, fitted learner models are discarded after the prediction in order to save
#'   some memory.
#' @return [BenchmarkResult].
#'
#' @section Parallelization:
#' This function can be parallelized with the \CRANpkg{future} package.
#' Each row in the `design` creates as many jobs as there are resampling iterations.
#' All jobs are forwarded to the \CRANpkg{future} package together.
#' To select a parallel backend, use [future::plan()].
#'
#' @note
#' The fitted models are discarded after the experiment has been scored in order to reduce memory consumption.
#' If you need access to the models for later analysis, set `store_model` to `TRUE` via [mlr_control()].
#'
#' @export
#' @examples
#' # benchmarking with expand_grid()
#' tasks = mlr_tasks$mget(c("iris", "sonar"))
#' learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
#' resamplings = mlr_resamplings$mget("holdout")
#'
#' design = expand_grid(tasks, learners, resamplings)
#' print(design)
#'
#' set.seed(123)
#' bmr = benchmark(design)
#'
#' ## performance for all conducted experiments
#' head(as.data.table(bmr))
#'
#' ## aggregated performance values
#' bmr$aggregated(objects = FALSE)
#'
#' ## Extract predictions of first experiment of this resampling
#' head(as.data.table(rr$experiment(1)$prediction))
#'
#' # benchmarking with a custom design:
#' # - fit classif.featureless on iris with a 3-fold CV
#' # - fit classif.rpart on sonar using a holdout
#' design = data.table::data.table(
#'   task = mlr_tasks$mget(c("iris", "sonar")),
#'   learner = mlr_learners$mget(c("classif.featureless", "classif.rpart")),
#'   resampling = mlr_resamplings$mget(c("cv3", "holdout"))
#' )
#'
#' ## instantiate resamplings
#' design$resampling = Map(
#'   function(task, resampling) resampling$clone()$instantiate(task),
#'   task = design$task, resampling = design$resampling
#' )
#'
#' ## calculate experiments
#' bmr = benchmark(design)
#' print(bmr)
#'
#' ## get the training set of the 2nd iteration of the featureless learner on iris
#' rr = bmr$aggregated()[learner_id == "classif.featureless"]$resample_result[[1]]
#' rr$experiment(2)$train_set
benchmark = function(design, ctrl = list()) {
  assert_data_frame(design, min.rows = 1L)
  assert_names(names(design), permutation.of = c("task", "learner", "resampling"))
  assert_tasks(design$task)
  assert_learners(design$learner)
  assert_resamplings(design$resampling, instantiated = TRUE)
  ctrl = mlr_control(ctrl)

  # clone inputs
  setDT(design)
  task = resampling = NULL
  design[, "task" := list(list(task[[1L]]$clone())), by = list(hashes(task))]
  design[, "resampling" := list(list(resampling[[1L]]$clone())), by = list(hashes(resampling))]

  # expand the design: add rows for each resampling iteration
  grid = pmap_dtr(design, function(task, learner, resampling) {
        hash = hash(task$hash, learner$hash, resampling$hash)
    data.table(task = list(task), learner = list(learner), resampling = list(resampling),
      iteration = seq_len(resampling$iters), hash = hash)
  })

  lg$info("Benchmarking %i experiments", nrow(grid))

  if (use_future()) {
    lg$debug("Running benchmark() via future")

    res = future.apply::future_mapply(workhorse,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iteration, MoreArgs = list(ctrl = ctrl), SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3"
    )
  } else {
    lg$debug("Running benchmark() sequentially")

    res = mapply(workhorse,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iteration, MoreArgs = list(ctrl = ctrl), SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  }

  res = rbindlist(Map(reassemble, row = res, learner = grid$learner), use.names = TRUE)
  res = insert_named(grid, res)

  lg$info("Finished benchmark")
  BenchmarkResult$new(res)
}
