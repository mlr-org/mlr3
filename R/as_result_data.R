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
#' @param predictions (list of list of [Prediction]s).
#' @param learner_states (`list()`)\cr
#'   Learner states. If not provided, the states of `learners` are automatically extracted.
#' @param data_extra (`list()`)\cr
#'  Additional data for each iteration.
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
#'   predictions[[i]] = list(test = l$predict(task, row_ids = resampling$test_set(i)))
#' }
#'
#' rdata = as_result_data(task, learners, resampling, iterations, predictions)
#' ResampleResult$new(rdata)
as_result_data = function(
  task,
  learners,
  resampling,
  iterations,
  predictions,
  learner_states = NULL,
  data_extra = NULL,
  store_backends = TRUE
  ) {
  assert_task(task)
  assert_learners(learners, task = task)
  assert_resampling(resampling, instantiated = TRUE)
  assert_integer(iterations, any.missing = FALSE, lower = 1L, upper = resampling$iters, unique = TRUE)
  assert_list(predictions, types = "list")
  assert_list(learner_states, null.ok = TRUE)
  assert_list(data_extra, null.ok = TRUE)
  predictions = map(predictions, function(x) map(x, as_prediction_data))

  N = length(iterations)

  if (length(learners) != N) {
    stopf("Number of learners (%i) must match the number of resampling iterations (%i)", length(learners), N)
  }

  if (length(predictions) != N) {
    stopf("Number of predictions (%i) must match the number of resampling iterations (%i)", length(predictions), N)
  }

  if (is.null(learner_states)) {
    learner_states = map(learners, "state")
  } else if (length(learner_states) != N) {
    stopf("Number of learner_states (%i) must match the number of resampling iterations (%i)", length(learner_states), N)
  }

  if (resampling$task_hash != task$hash) {
    stopf("Resampling '%s' has not been trained on task '%s', hashes do not match", resampling$id, task$id)
  }

  if (!is.null(data_extra) && length(data_extra) != N) {
    stopf("Length of data_extra (%i) must match the number of resampling iterations (%i)", length(data_extra), N)
  }

  ResultData$new(data.table(
    task = list(task),
    learner = learners,
    learner_hash = hashes(learners),
    learner_state = learner_states,
    param_values = map(learners, function(x) x$param_set$values),
    resampling = list(resampling),
    iteration = iterations,
    prediction = predictions,
    uhash = UUIDgenerate()
  ), data_extra = data_extra, store_backends = store_backends)
}
