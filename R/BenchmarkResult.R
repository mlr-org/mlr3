#' @title Container for Results of benchmark
#'
#' @description
#' This is the object returned by [benchmark].
#'
#' @section Usage:
#'
#' ```
#' bmr$tasks
#' bmr$learners
#' bmr$resamplings
#' bmr$measures
#' bmr$performance
#' bmr$aggregated
#' bmr$resample_results
#' bmr$resample_result(hash)
#' ```
#'
#' @section Arguments:
#' * `hash` \[`character(1)`\]:\cr
#'   String which identifies a subgroup to extract as [ResampleResult].
#'
#' @section Details:
#' * `$tasks`, `$learners`, `$resamplings` and `$measures` return an overview table of involved objects.
#'
#' * `$performance` provides a [`data.table()`][data.table::data.table()] with column `iteration` (integer) and a numeric column for each
#'   performance measure (columns named using the measure ids).
#'
#' * `$aggregated` returns aggregated performance measures as a [`data.table()`][data.table::data.table()].
#'   The table is build similar to the one returned by `$performance`, but experiments are aggregated by their resample result group
#'   (combination of [Task], [Learner] and [Resampling]). The actual aggregation function is defined by the respective [Measure].
#'
#' * `$resample_results` returns a [`data.table()`][data.table::data.table()] which gives an overview of the resample result groups in the benchmark.
#'   These groups in the [BenchmarkResult] can be extracted as [ResampleResult] for further inspection.
#'
#' * `$resample_result()` creates the [ResampleResult] identified by the specified `hash` value.
#'
#' @name BenchmarkResult
#' @examples
#' bmr = benchmark(
#'   tasks = mlr_tasks$mget("iris"),
#'   learners = mlr_learners$mget(c("classif.dummy", "classif.rpart")),
#'   resamplings = mlr_resamplings$mget("cv"),
#'   ctrl = exec_control(verbose = FALSE)
#' )
#'
#' bmr$tasks
#' bmr$learners
#' bmr$resamplings
#' bmr$measures
#' bmr$performance
#' bmr$aggregated
#' rrs = bmr$resample_results
#' print(rrs)
#' rr = bmr$resample_result(rrs$hash[1])
#' print(rr)
#' rr$experiment(1)$model
NULL

BenchmarkResult = R6Class("BenchmarkResult",
  cloneable = FALSE,
  public = list(
    data = NULL,

    initialize = function(data) {
      assert_data_table(data)
      slots = mlr_reflections$experiment_slots$name
      assert_names(names(data), permutation.of = c(slots, "hash"))
      self$data = setcolorder(data, slots)
      setkeyv(self$data, "hash")
    },

    resample_result = function(hash) {
      assert_string(hash)
      assert_choice(hash, self$data[, unique(hash)])
      tmp = hash
      ResampleResult$new(self$data[hash == tmp], hash = hash) # FIXME: join
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

    performance = function() {
      flatten(self$data[, list(hash = hash, task_id = ids(task), learner_id = ids(learner),
        resampling_id = ids(resampling), performance = performance)], "performance")
    },

    aggregated = function() {
      collect = function(data) as.list(ResampleResult$new(data)$aggregated)
      res = self$data[, list(task_id = task[[1L]]$id, learner_id = learner[[1L]]$id, resampling_id = resampling[[1L]]$id, performance = list(collect(.SD))), by = hash]
      flatten(res, "performance")
    }
  )
)
