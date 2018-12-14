#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel).
#'
#' @param task ([Task]):\cr
#'   Object of type [Task].
#' @param learner ([Learner]):\cr
#'   Object of type [Learner].
#' @param resampling ([Resampling]):\cr
#'   Object of type [Resampling].
#' @param measures (`list` of [Measure]):\cr
#'   List of performance measures used to assess the predictive performance.
#'   Defaults to the measures stored in `task`.
#' @param ctrl (named `list()`, e.g. as returned by [mlr_control()]):\cr
#'   Object to control various parts of the execution. See [mlr_control()].
#' @return [ResampleResult].
#' @export
#' @examples
#' \dontshow{
#'    set.seed(123)
#'    .threshold = logger::log_threshold(namespace = "mlr3")
#'    logger::log_threshold(logger::WARN, namespace = "mlr3")
#' }
#' task = mlr_tasks$get("iris")
#' learner = mlr_learners$get("classif.rpart")
#' resampling = mlr_resamplings$get("cv")
#' rr = resample(task, learner, resampling)
#' print(rr)
#' rr$aggregated
#' rr$performance("mmce")
#'
#' # Repeat resampling with featureless learner and combine
#' # the ResampleResults into a BenchmarkResult
#' learner = mlr_learners$get("classif.featureless")
#' rr.featureless = resample(task, learner, resampling)
#'
#' bmr = rr$combine(rr.featureless)
#' bmr$aggregated
#' \dontshow{
#'    logger::log_threshold(.threshold, namespace = "mlr3")
#' }
resample = function(task, learner, resampling, measures = NULL, ctrl = list()) {
  assert_task(task)
  assert_learner(learner, task = task)
  assert_resampling(resampling)
  if (is.null(measures))
    measures = task$measures
  assert_measures(measures, task = task)
  ctrl = mlr_control(ctrl)

  if (resampling$is_instantiated) {
    instance = resampling$clone()
  } else {
    instance = resampling$instantiate(task)
  }
  n = instance$iters

  if (future_remote()) {
    log_debug("Running resample() via future with %i iterations", n, namespace = "mlr3")
    res = future.apply::future_lapply(seq_len(n), experiment_worker,
      task = task, learner = learner, resampling = resampling, measures = measures, ctrl = ctrl,
      remote = TRUE, future.globals = FALSE, future.packages = "mlr3")
  } else {
    log_debug("Running resample() sequentially with %i iterations", n, namespace = "mlr3")
    res = lapply(seq_len(n), experiment_worker,
      task = task, learner = learner, resampling = resampling, measures = measures, ctrl = ctrl)
  }

  res = combine_experiments(res)
  res[, c("task", "resampling", "measures") := list(list(task), list(instance), list(measures))]
  ResampleResult$new(res)
}
