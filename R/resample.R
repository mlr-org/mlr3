#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel).
#'
#' @param task ([Task])\cr
#'   Object of type [Task].
#' @param learner ([Learner])\cr
#'   Object of type [Learner].
#' @param resampling ([Resampling])\cr
#'   Object of type [Resampling].
#' @param measures (list of [Measure])\cr
#'   List of performance measures used to assess the predictive performance.
#'   Defaults to the measures stored in `task`.
#' @return [ResampleResult].
#' @export
#' @examples
#' task = mlr_tasks$get("iris")
#' learner = mlr_learners$get("classif.rpart")
#' resampling = mlr_resamplings$get("cv")
#' rr = resample(task, learner, resampling)
#' print(rr)
#' rr$aggregated
#' rr$performance
#'
#' # Repeat resampling with dummy learner and combine
#' # the ResampleResults into a BenchmarkResult
#' learner = mlr_learners$get("classif.dummy")
#' rr.dummy = resample(task, learner, resampling)
#'
#' bmr = rr$combine(rr.dummy)
#' bmr$performance[, list(mean.mmce = mean(mmce)), by = "learner"]
resample = function(task, learner, resampling, measures = NULL) {
  assert_task(task)
  assert_learner(learner, task = task)
  assert_resampling(resampling)
  if (is.null(measures))
    measures = task$measures
  assert_measures(measures, task = task, learner = learner)

  if (resampling$is_instantiated) {
    instance = resampling$clone()
  } else {
    instance = resampling$instantiate(task)
  }
  n = instance$iters

  if (use_future()) {
    debug("Running resample() via future with %i iterations", n)
    res = future.apply::future_lapply(seq_len(n), experiment_worker,
      task = task, learner = learner, resampling = resampling, measures = measures, ctrl = mlr_options(),
      future.globals = FALSE, future.packages = "mlr3")
  } else {
    debug("Running resample() sequentially with %i iterations", n)
    res = lapply(seq_len(n), experiment_worker,
      task = task, learner = learner, resampling = resampling, measures = measures, ctrl = mlr_options())
  }

  res = combine_experiments(res)
  res[, c("task", "learner", "resampling", "measures") := list(list(task), list(learner), list(instance), list(measures))]
  ResampleResult$new(res)
}
