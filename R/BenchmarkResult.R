#' @title Container for Results of `benchmark()`
#'
#' @description
#' This is the object returned by [benchmark()].
#'
#' @section Usage:
#'
#' ```
#' bmr$tasks
#' bmr$learners
#' bmr$resamplings
#' bmr$measures
#' bmr$aggregated
#' bmr$resample_results
#' bmr$resample_result(hash)
#' as.data.table(bmr)
#' ```
#'
#' @section Arguments:
#' * `hash` (`character(1)`):\cr
#'   String which identifies a subgroup to extract as [ResampleResult].
#' * `bmr` ([BenchmarkResult]):\cr
#'   String which identifies a subgroup to extract as [ResampleResult].
#'
#' @section Details:
#' * `$tasks`, `$learners`, `$resamplings` and `$measures` return an overview table of involved objects.
#'
#' * `$aggregated` returns aggregated performance measures as a [data.table::data.table()].
#'   Experiments are aggregated by their resample result group
#'   (combination of [Task], [Learner] and [Resampling]).
#'  The actual aggregation function is defined by the respective [Measure].
#'
#' * `$resample_results` returns a [data.table::data.table()] which gives an overview of the
#'   resample result groups in the benchmark.
#'   These groups in the [BenchmarkResult] can be extracted as [ResampleResult] for further inspection.
#'
#' * `$resample_result()` creates the [ResampleResult] identified by the specified `hash` value.
#'
#' * `as.data.table()` converts a [BenchmarkResult] to a [data.table::data.table()].
#'
#' @name BenchmarkResult
#' @examples
#' \dontshow{
#'    set.seed(123)
#'    .threshold = logger::log_threshold(namespace = "mlr3")
#'    logger::log_threshold(logger::WARN, namespace = "mlr3")
#' }
#' bmr = benchmark(
#'   tasks = mlr_tasks$mget("iris"),
#'   learners = mlr_learners$mget(c("classif.featureless", "classif.rpart")),
#'   resamplings = mlr_resamplings$mget("cv")
#' )
#'
#' bmr$tasks
#' bmr$learners
#' bmr$resamplings
#' bmr$measures
#' bmr$aggregated
#' rrs = bmr$resample_results
#' print(rrs)
#' rr = bmr$resample_result(rrs$hash[1])
#' print(rr)
#' rr$experiment(1)$model
#' \dontshow{
#'    logger::log_threshold(.threshold, namespace = "mlr3")
#' }
NULL

#' @export
BenchmarkResult = R6Class("BenchmarkResult",
  public = list(
    data = NULL,

    initialize = function(data) {
      assert_data_table(data)
      slots = mlr_reflections$experiment_slots$name
      assert_names(names(data), must.include = c(slots, "hash"))
      self$data = setcolorder(data, slots)
    },

    resample_result = function(hash) {
      assert_string(hash)
      assert_choice(hash, self$data[, unique(hash)])
      tmp = hash
      ResampleResult$new(self$data[list(tmp), on = "hash", nomatch = 0L], hash = hash)
    },

    combine = function(bmr) {
      assert_benchmark_result(bmr)
      self$data = rbindlist(list(self$data, bmr$data), fill = TRUE)
      invisible(self)
    },

    get_best = function(measure) {
      assert_measure(measure)
      id = measure$id
      aggr = self$aggregated
      if (id %nin% names(aggr))
        stopf("Measure with id '%s' not in BenchmarkResult", id)
      best = if (measure$minimize) which_min(aggr[[id]]) else which_max(aggr[[id]])
      aggr$resample_result[[best]]
    }
  ),

  active = list(
    tasks = function() {
      unique(self$data[, list(task_hash = hashes(task), task_id = ids(task), task = task)], by = "task_hash")
    },

    learners = function() {
      unique(self$data[, list(learner_hash = hashes(learner), learner_id = ids(learner), learner = learner)], by = "learner_hash")
    },

    resamplings = function() {
      unique(self$data[, list(resampling_hash = hashes(resampling), resampling_id = ids(resampling), resampling = resampling)], by = "resampling_hash")
    },

    measures = function() {
      unique(map_dtr(self$data$measures, function(m) data.table(measure_id = ids(m), measure = m)), by = "measure_id")
    },

    resample_results = function() {
      self$data[, list(task_id = task[[1L]]$id, learner_id = learner[[1L]]$id, resampling_id = resampling[[1L]]$id, .N), by = "hash"]
    },

    aggregated = function() {
      extract = function(x) {
        c(list(task_id = x$task$id, learner_id = x$learner$id, resampling_id = x$resampling$id),
          as.list(x$aggregated))
      }
      res = self$data[, list(resample_result = list(ResampleResult$new(.SD))), by = hash]
      ref_cbind(res, map_dtr(res$resample_result, extract, .fill = TRUE))
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "data") copy(value) else value
    }
  )
)

#' @export
as.data.frame.BenchmarkResult = function(x, ...) {
  setDF(as.data.table.BenchmarkResult(x))[]
}

#' @export
as.data.table.BenchmarkResult = function(x, ...) {
  hash = task = learner = resampling = performance = NULL
  unnest(x$data[,
    list(
      hash = hash,
      task = task, task_id = ids(task),
      learner = learner, learner_id = ids(learner),
      resampling = resampling, resampling_id = ids(resampling),
      performance = performance
    )
  ], "performance")
}
