#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel):
#' Repeatedly apply [Learner] `learner` on a training set of [Task] `task` to train a model,
#' then use the trained model to predict observations of a test set.
#' Training and test sets are defined by the [Resampling] `resampling`.
#'
#' @param task ([Task]).
#' @param learner ([Learner]).
#' @param resampling ([Resampling]).
#' @param store_models (`logical(1)`)\cr
#'   Keep the fitted model after the test set has been predicted?
#'   Set to `TRUE` if you want to further analyse the models or want to
#'   extract information like variable importance.
#' @return [ResampleResult].
#'
#'
#' @template section_parallelization
#' @template section_progress_bars
#' @template section_logging
#'
#' @note
#' The fitted models are discarded after the predictions have been computed in order to reduce memory consumption.
#' If you need access to the models for later analysis, set `store_models` to `TRUE`.
#'
#' @export
#' @examples
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv")
#'
#' # Explicitly instantiate the resampling for this task for reproduciblity
#' set.seed(123)
#' resampling$instantiate(task)
#'
#' rr = resample(task, learner, resampling)
#' print(rr)
#'
#' # Retrieve performance
#' rr$score(msr("classif.ce"))
#' rr$aggregate(msr("classif.ce"))
#'
#' # merged prediction objects of all resampling iterations
#' pred = rr$prediction()
#' pred$confusion
#'
#' # Repeat resampling with featureless learner
#' rr_featureless = resample(task, lrn("classif.featureless"), resampling)
#'
#' # Convert results to BenchmarkResult, then combine them
#' bmr1 = as_benchmark_result(rr)
#' bmr2 = as_benchmark_result(rr_featureless)
#' print(bmr1$combine(bmr2))
resample = function(task, learner, resampling, store_models = FALSE) {
  task = assert_task(as_task(task, clone = TRUE))
  learner = assert_learner(as_learner(learner, clone = TRUE))
  resampling = assert_resampling(as_resampling(resampling))
  assert_flag(store_models)
  assert_learnable(task, learner)

  instance = resampling$clone(deep = TRUE)
  if (!instance$is_instantiated) {
    instance = instance$instantiate(task)
  }
  n = instance$iters
  pb = get_progressor(n)

  lg$debug("Running resample() via future with %i iterations", n)

  res = future.apply::future_lapply(seq_len(n), workhorse,
    task = task, learner = learner, resampling = instance,
    store_models = store_models, lgr_threshold = lg$threshold, pb = pb,
    future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
    future.packages = "mlr3", future.seed = TRUE
  )

  rdata = rdata_from_table(data.table(
    task = list(task),
    learner = list(learner),
    learner_state = map(res, "learner_state"),
    resampling = list(instance),
    iteration = seq_len(n),
    prediction = map(res, "prediction"),
    uhash = UUIDgenerate()
  ))

  ResampleResult$new(data = rdata)
}
