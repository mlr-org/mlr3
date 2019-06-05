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
#'   The helper function [expand_grid()] can assist in generating an exhaustive design (see examples) and properly
#'   instantiate the [Resampling]s per [Task].
#' @param measures :: list of [Measure]\cr
#'   List of performance measures to calculate.
#'   Defaults to the measures specified in the each respective [Task].
#'   The measures will be cloned.
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
#' # performance for all conducted experiments
#' head(as.data.table(bmr))
#'
#' # aggregated performance values
#' bmr$aggregated(objects = FALSE)
#'
#' # Overview of of resamplings that were conducted internally
#' aggr = bmr$aggregated()
#' print(aggr)
#'
#' # Extract first ResampleResult
#' rr = aggr[1, resample_result][[1]]
#' print(rr)
#'
#' # Extract predictions of first experiment of this resampling
#' head(as.data.table(rr$experiment(1)$prediction))
benchmark = function(design, measures = NULL, ctrl = list()) {

  assert_data_frame(design, min.rows = 1L)
  assert_names(names(design), permutation.of = c("task", "learner", "resampling"))
  assert_list(design$task, "Task")
  assert_list(design$learner, "Learner")
  assert_list(design$resampling, "Resampling")
  if (!is.null(measures)) {
    measures = assert_measures(measures, clone = TRUE)
  }
  ctrl = mlr_control(ctrl)
  is_exhautive_grid = isTRUE(attr(design, "exhaustive_grid"))

  # clone inputs
  task = learner = NULL
  design[, "task" := list(list(task[[1L]]$clone())), by = list(hashes(task))]

  # expand the design: add rows for each resampling iteration
  grid = pmap_dtr(design, function(task, learner, resampling) {
    if (!is_exhautive_grid) {
      resampling = resampling$clone()
      if (!resampling$is_instantiated) {
        resampling$instantiate(task)
      }
    }
    hash = experiment_data_hash(list(task = task, learner = learner, resampling = resampling))
    data.table(task = list(task), learner = list(learner), resampling = list(resampling),
      measures = list(measures %??% task$measures), iter = seq_len(resampling$iters), hash = hash)
  })

  lg$info("Benchmarking %i experiments", nrow(grid))

  if (use_future()) {
    lg$debug("Running benchmark() via future")

    tmp = future.apply::future_mapply(experiment_worker,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iter, measures = grid$measures,
      MoreArgs = list(ctrl = ctrl, remote = TRUE), SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3"
    )
  } else {
    lg$debug("Running benchmark() sequentially")

    tmp = mapply(experiment_worker,
      task = grid$task, learner = grid$learner, resampling = grid$resampling,
      iteration = grid$iter, measures = grid$measures,
      MoreArgs = list(ctrl = ctrl, remote = FALSE), SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  }

  combined = combine_experiments(tmp)

  # this is required to get a clean learner object:
  # during parallelization, learners might get serialized and are getting unnecessarily big
  # after de-serialization
  # insert_named(combined, list(learner = copy_models(combined$learner, grid$learner)))

  grid = ref_cbind(remove_named(grid, c("iter", "learner")), combined)
  lg$info("Finished benchmark")
  BenchmarkResult$new(grid)
}

#' @title Generate a Benchmark Design
#'
#' @description
#' Takes a lists of [Task], a list of [Learner] and a list of [Resampling] to
#' generate a design in an [expand.grid()] fashion (a.k.a. cross join or Cartesian product).
#'
#' Resampling strategies may not be instantiated, and will be instantiated per task internally.
#'
#' @param tasks :: (list of [Task] | `character()`)\cr
#'   Instead a [Task] object, it is also possible to provide a keys to retrieve tasks from the [mlr_tasks] dictionary.
#' @param learners (list of [Learner] | `character()`)\cr
#'   Instead if a [Learner] object, it is also possible to provide keys to retrieve learners from the [mlr_learners] dictionary.
#' @param resamplings :: (list of [Resampling] | `character()`)\cr
#'   Instead if a [Resampling] object, it is also possible to provide a key to retrieve a resampling from the [mlr_resamplings] dictionary.
#'
#' @return ([data.table()]) with the cross product of the input vectors.
#' @export
expand_grid = function(tasks, learners, resamplings) {

  tasks = assert_tasks(tasks)
  learners = assert_learners(learners)
  resamplings = assert_resamplings(resamplings)
  assert_resamplings(resamplings, instantiated = FALSE)

  grid = CJ(task = seq_along(tasks), resampling = seq_along(resamplings))
  instances = pmap(grid, function(task, resampling) resamplings[[resampling]]$clone()$instantiate(tasks[[task]]))
  grid$instance = seq_row(grid)
  grid = grid[CJ(task = seq_along(tasks), learner = seq_along(learners)), on = "task", allow.cartesian = TRUE]

  design = data.table(task = tasks[grid$task], learner = learners[grid$learner], resampling = instances[grid$instance])
  attr(design, "exhaustive_grid") = TRUE
  design
}
