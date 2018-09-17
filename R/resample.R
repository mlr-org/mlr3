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
resample = function(task, learner, resampling) {
  assert_task(task)
  assert_learner(learner, task = task)
  assert_resampling(resampling)

  if (resampling$is_instantiated) {
    instance = resampling$clone()
  } else {
    instance = resampling$instantiate(task)
  }
  n = instance$iters

  res = future.apply::future_lapply(seq_len(n), experiment_worker,
    task = task, learner = learner, resampling = resampling,
    ctrl = mlr_options(),
    future.globals = FALSE, future.packages = "mlr3")
  res = combine_experiments(res)
  res[, c("task", "resampling") := list(list(task), list(instance))]

  ResampleResult$new(res)
}
