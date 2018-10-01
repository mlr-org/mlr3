#' @title Container for Results of benchmark
#'
#' @description
#' This is the object returned by [benchmark].
#'
#' @section Usage:
#'
#' ```
#' rr$experiment(iter)
#' rr$experiments(iters)
#' bmr$performance
#' bmr$aggregated
#' bmr$resample_results
#' bmr$resample_result(hash)
#' ```
#'
#' @section Arguments:
#' * `hash` (`character(1)`):
#'   String which identifies a subgroup to extract as [ResampleResult].
#'
#' @section Details:
#' `$experiment()` returns an [Experiment] for the `iter`-th resampling iteration.
#'
#' `$experiments()` returns a list with the slice of [Experiment]s for the provided `iters`.
#'
#' `$performance` provides a [data.table::data.table()] with column `iteration` (integer) and a numeric column for each
#'   performance measure (columns named using the measure ids).
#'
#' `$aggregated` returns the aggregated performance measures. The aggregation method is part of the [Measure].
#'
#' `$resample_results` returns a [data.table::data.table()] which gives an overview of the subgroups in the benchmark.
#'   Groups of experiments in the [BenchmarkResult] can be extracted as [ResampleResult].
#'
#' `$resample_result()` creates the [ResampleResult] identified by the specified `hash` value.
#'
#' @name BenchmarkResult
NULL

BenchmarkResult = R6Class("BenchmarkResult",
  cloneable = FALSE,
  public = list(
    data = NULL,

    initialize = function(data) {
      assert_data_table(data)
      slots = reflections$experiment_slots$name
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
    resample_results = function() {
      res = self$data[, list(task = task[[1L]]$id, learner = learner[[1L]]$id, resampling = resampling[[1L]]$id, .N), by = "hash"]
      setorderv(res, c("task", "learner", "resampling"))[]
    },

    performance = function() {
      res = self$data[, list(task = ids(task), learner = ids(learner), resampling = ids(resampling), hash = hash, performance = performance)]
      setorderv(res, c("task", "learner", "resampling"))
      rcbind(res[, !"performance"], rbindlist(res$performance, fill = TRUE))[]
    }
  )
)
