#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel).
#'
#' @param task ([Task]):
#'   Object of type [Task].
#'   Instead if a [Task] object, it is also possible to provide a key to retrieve a task from the [mlr_tasks] dictionary.
#' @param learner ([Learner]):
#'   Object of type [Learner].
#'   Instead if a [Learner] object, it is also possible to provide a key to retrieve a task from the [mlr_learners] dictionary.
#' @param resampling ([Resampling]):
#'   Object of type [Resampling].
#'   Instead if a [Resampling] object, it is also possible to provide a key to retrieve a task from the [mlr_resamplings] dictionary.
#' @param measures (list of [Measure]):
#'   List of performance measures to calculate.
#'   Defaults to the measures specified in the [Task] `task`.
#' @param ctrl (named `list()`, e.g. as returned by [mlr_control()]):
#'   Object to control various parts of the execution. See [mlr_control()].
#' @return [ResampleResult].
#' @export
#' @examples
#' \dontshow{
#' .threshold = logger::log_threshold(namespace = "mlr3")
#' logger::log_threshold(logger::WARN, namespace = "mlr3")
#' }
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
#' \dontshow{
#' logger::log_threshold(.threshold, namespace = "mlr3")
#' }
resample = function(task, learner, resampling, measures = NULL, ctrl = list()) {

  task = assert_task(task, clone = TRUE)
  learner = assert_learner(learner, task = task, clone = TRUE)
  resampling = assert_resampling(resampling)
  measures = assert_measures(measures %??% task$measures, task = task, clone = TRUE)
  ctrl = mlr_control(ctrl)

  instance = resampling$clone(deep = TRUE)
  if (!instance$is_instantiated) {
    instance = instance$instantiate(task)
  }
  n = instance$iters

  if (use_future()) {
    log_debug("Running resample() via future with %i iterations", n, namespace = "mlr3")
    res = future.apply::future_lapply(seq_len(n), experiment_worker,
      task = task, learner = learner, resampling = instance, measures = measures, ctrl = ctrl,
      remote = TRUE, future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3")
  } else {
    log_debug("Running resample() sequentially with %i iterations", n, namespace = "mlr3")
    res = lapply(seq_len(n), experiment_worker,
      task = task, learner = learner, resampling = instance, measures = measures, ctrl = ctrl)
  }

  res = combine_experiments(res)
  res[, c("task", "learner", "resampling", "measures") := list(list(task), list(learner), list(instance), list(measures))]
  ResampleResult$new(res)
}
