#' @title Convert to ResultData
#'
#' @description
#' This function allows to construct or convert to a [ResultData] object, the
#' result container used by [ResampleResult] and [BenchmarkResult].
#' A [ResampleResult] or [BenchmarkResult] can be initialized with the returned object.
#' Note that [ResampleResult]s can be converted to a [BenchmarkResult] with [as_benchmark_result()]
#' and multiple [BenchmarkResult]s can be combined to a larger [BenchmarkResult] with the
#' `$combine()` method of [BenchmarkResult].
#'
#' @param task ([Task]).
#' @param learners (list of trained [Learner]s).
#' @param resampling ([Resampling]).
#' @param iterations (`integer()`).
#' @param predictions (list of [Prediction]s).
#' @param learner_states (`list()`)\cr
#'   Learner states. If not provided, the states of `learners` are automatically extracted.
#' @param store_backends (`logical(1)`)\cr
#'   If set to `FALSE`, the backends of the [Task]s provided in `data` are
#'   removed.
#'
#' @return `ResultData` object which can be passed to the constructor of [ResampleResult].
#' @export
#' @examples
#' task = tsk("penguins")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv", folds = 2)$instantiate(task)
#' iterations = seq_len(resampling$iters)
#'
#' # manually train two learners.
#' # store learners and predictions
#' learners = list()
#' predictions = list()
#' for (i in iterations) {
#'   l = learner$clone(deep = TRUE)
#'   learners[[i]] = l$train(task, row_ids = resampling$train_set(i))
#'   predictions[[i]] = l$predict(task, row_ids = resampling$test_set(i))
#' }
#'
#' rdata = as_result_data(task, learners, resampling, iterations, predictions)
#' ResampleResult$new(rdata)
as_result_data = function(task, learners, resampling, iterations, predictions, learner_states = NULL, store_backends = TRUE) {
  assert_integer(iterations, any.missing = FALSE, lower = 1L, upper = resampling$iters, unique = TRUE)
  n = length(iterations)

  assert_task(task)
  assert_learners(learners, task = task)
  assert_resampling(resampling, instantiated = TRUE)
  predictions = lapply(predictions, as_prediction_data)
  uhash = UUIDgenerate()

  if (is.null(learner_states)) {
    learner_states = map(learners, "state")
  }

  ResultData$new(data.table(
    task = list(task),
    learner = learners,
    learner_state = learner_states,
    resampling = list(resampling),
    iteration = iterations,
    prediction = predictions,
    uhash = uhash
  ), store_backends = store_backends)
}
