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
#' bmr$resample_result(i)
#' bmr$performance
#' bmr$aggregated
#' ```
#'
#' @section Arguments:
#' * `i` (`integer`):
#'   Counter of the [ResampleResult] to retrieve.
#'
#' @section Details:
#' `$experiment()` returns an [Experiment] for the `iter`-th resampling iteration.
#'
#' `$experiments()` returns a list with the slice of [Experiment]s for the provided `iters`.
#'
#' `$performance` provides a [data.table::data.table] with column `iteration` (integer) and a numeric column for each
#'   performance measure (columns named using the measure ids).
#'
#' `$aggregated` returns the aggregated performance measures. The aggregation method is part of the [Measure].
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
    },

    resampling = function(hash) {
      assert_string(hash)
      assert_choice(hash, self$data[, unique(hash)])
      tmp = hash
      ResampleResult$new(self$data[hash == tmp], hash = hash)
    }
  ),

  active = list(
    hashes = function() {
      res = self$data[, list(task = task[[1L]]$id, learner = learner[[1L]]$id, resampling = resampling[[1L]]$id, .N), by = "hash"]
      setcolorder(res, c("hash", "task", "learner", "resampling"))[]
    },

    performance = function() {
      tmp = self$data[, list(task = ids(task), learner = ids(learner), performance = performance)]
      cbind(tmp[, !"performance"], rbindlist(tmp$performance, fill = TRUE))
    }
  )
)
