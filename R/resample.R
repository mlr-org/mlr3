#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel).
#'
#' @param task :: [Task]\cr
#'   See also [mlr_sugar].
#' @param learner :: [Learner]\cr
#'   See also [mlr_sugar].
#' @param resampling :: [Resampling]\cr
#'   See also [mlr_sugar].
#' @param store_models :: `logical(1)`\cr
#'   Keep the fitted model after the test set has been predicted?
#'   Set to `TRUE` if you want to further analyse the models or want to
#'   extract information like variable importance.
#' @return [ResampleResult].
#'
#'
#' @template section-parallelization
#' @template section-logging
#'
#' @note
#' The fitted models are discarded after the predictions have been scored in order to reduce memory consumption.
#' If you need access to the models for later analysis, set `store_models` to `TRUE`.
#'
#' @export
#' @examples
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsp("cv")
#'
#' # explicitly instantiate the resampling for this task for reproduciblity
#' set.seed(123)
#' resampling$instantiate(task)
#'
#' rr = resample(task, learner, resampling)
#' print(rr)
#'
#' # retrieve performance
#' rr$performance("classif.ce")
#' rr$aggregate("classif.ce")
#'
#' # merged prediction objects of all resampling iterations
#' pred = rr$prediction
#' pred$confusion
#'
#' # Repeat resampling with featureless learner
#' rr.featureless = resample(task, "classif.featureless", resampling)
#'
#' # Convert results to BenchmarkResult, then combine them
#' bmr1 = as_benchmark_result(rr)
#' bmr2 = as_benchmark_result(rr.featureless)
#' print(bmr1$combine(bmr2))
resample = function(task, learner, resampling, store_models = FALSE) {
  task = assert_task(task, clone = TRUE)
  learner = assert_learner(learner, task = task, properties = task$properties, clone = TRUE)
  resampling = assert_resampling(resampling)
  assert_flag(store_models)

  instance = resampling$clone(deep = TRUE)
  if (!instance$is_instantiated) {
    instance = instance$instantiate(task)
  }
  n = instance$iters

  if (use_future()) {
    lg$debug("Running resample() via future with %i iterations", n)
    res = future.apply::future_lapply(seq_len(n), workhorse,
      task = task, learner = learner, resampling = instance, store_models = store_models, lgr_threshold = lg$threshold,
      future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
      future.packages = "mlr3")
  } else {
    lg$debug("Running resample() sequentially with %i iterations", n)
    res = lapply(seq_len(n), workhorse,
      task = task, learner = learner, resampling = instance, store_models = store_models)
  }

  res = map_dtr(res, reassemble, learner = learner)
  res[, c("task", "resampling", "iteration") := list(list(task), list(instance), seq_len(n))]

  ResampleResult$new(res)
}
