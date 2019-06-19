#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel).
#'
#' @param task :: ([Task] | `character(1)`)\cr
#'   Object of type [Task].
#'   Instead if a [Task] object, it is also possible to provide a key to retrieve a task from the [mlr_tasks] dictionary.
#' @param learner :: ([Learner] | `character(1)`)\cr
#'   Object of type [Learner].
#'   Instead if a [Learner] object, it is also possible to provide a key to retrieve a learner from the [mlr_learners] dictionary.
#' @param resampling :: ([Resampling] | `character(1)`)\cr
#'   Object of type [Resampling].
#'   Instead if a [Resampling] object, it is also possible to provide a key to retrieve a resampling from the [mlr_resamplings] dictionary.
#' @param measures :: list of [Measure]\cr
#'   List of performance measures to calculate.
#'   Defaults to the measures specified in the [Task] `task`.
#' @param ctrl :: named `list()`\cr
#'   Object to control experiment execution. See [mlr_control()] for details.
#' @return [ResampleResult].
#'
#' @section Parallelization:
#' This function can be parallelized with the \CRANpkg{future} package.
#' One jobs is one resampling iteration, and all jobs are forwarded to the \CRANpkg{future} package together.
#' To select a parallel backend, use [future::plan()].
#'
#' @note
#' The fitted models are discarded after the experiment has been scored in order to reduce memory consumption.
#' If you need access to the models for later analysis, set `store_model` to `TRUE` via [mlr_control()].
#'
#' @export
#' @examples
#' task = mlr_tasks$get("iris")
#' learner = mlr_learners$get("classif.rpart")
#' resampling = mlr_resamplings$get("cv")
#'
#' # explicitly instantiate the resampling for this task for reproduciblity
#' set.seed(123)
#' resampling$instantiate(task)
#'
#' rr = resample(task, learner, resampling)
#' print(rr, digits = 2)
#'
#' # retrieve performance
#' rr$performance("classif.ce")
#' rr$aggregated
#'
#' # merged prediction object for all experiments
#' pred = rr$prediction
#' pred$confusion
#'
#' # Repeat resampling with featureless learner
#' rr.featureless = resample(task, "classif.featureless", resampling)
#'
#' # Combine the ResampleResults into a BenchmarkResult
#' bmr = rr$combine(rr.featureless)
#' bmr$aggregated(objects = FALSE)
resample = function(task, learner, resampling, measures = NULL, ctrl = list()) {

  task = assert_task(task, clone = TRUE)
  learner = assert_learner(learner, task = task, clone = TRUE)
  resampling = assert_resampling(resampling)
  measures = assert_measures(measures %??% task$measures, task = task, learner = learner, clone = TRUE)
  ctrl = mlr_control(ctrl)

  instance = resampling$clone(deep = TRUE)
  if (!instance$is_instantiated) {
    instance = instance$instantiate(task)
  }
  n = instance$iters

  if (use_future()) {
    lg$debug("Running resample() via future with %i iterations", n)
    res = future.apply::future_lapply(seq_len(n), experiment_worker,
      task = task, learner = learner, resampling = instance, measures = measures, ctrl = ctrl,
      remote = TRUE, future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3")
  } else {
    lg$debug("Running resample() sequentially with %i iterations", n)
    res = lapply(seq_len(n), experiment_worker,
      task = task, learner = learner, resampling = instance, measures = measures, ctrl = ctrl)
  }

  res = combine_experiments(res)
  res[, c("task", "resampling", "measures") := list(list(task), list(instance), list(measures))]

  # this is required to get a clean objects on the master if calculation was performed in a remote session.
  if (use_future() || any(runs_remotely(ctrl))) {
    res = insert_named(res, list(learner = lapply(res$learner, function(l) l$reassemble())))
    res = insert_named(res, list(prediction = lapply(res$prediction, function(p) if (!is.null(p)) p$reassemble())))
  }

  ResampleResult$new(res)
}
