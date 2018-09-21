#' @title Container for Results of resample
#'
#' @description
#' This is the object returned by [resample].
#'
#' @section Usage:
#'
#' ```
#' rr$task
#' rr$learner
#' rr$resampling
#' rr$measures
#' rr$experiment(iter)
#' rr$experiments(iters)
#' rr$performance
#' rr$aggregated
#' ```
#'
#' @section Arguments:
#' * `i` (`integer`):
#'   Iteration(s) of the experiment(s) to retrieve.
#'
#' @section Details:
#' `$task`, `learner`, `resampling` and `measure` allow access to the [Task], [Learner], [Resampling] and [Measure] used in
#' the resampling.
#'
#' `$experiment()` returns an [Experiment] for the `iter`-th resampling iteration.
#'
#' `$experiments()` returns a list with the slice of [Experiment]s for the provided `iters`.
#'
#' `$performance` provides a [data.table::data.table] with column `iteration` (integer) and a numeric column for each
#'   performance measure (columns named using the measure ids).
#'
#' `$aggregated` returns the aggregated performance measures. The aggregation method is part of the [Measure].
#'
#' @name ResampleResult
NULL

ResampleResult = R6Class("ResampleResult",
  cloneable = FALSE,
  public = list(
    data = NULL,

    initialize = function(data) {
      assert_data_table(data)
      slots = reflections$experiment_slots$name
      assert_names(names(data), permutation.of = slots)
      self$data = setcolorder(data, slots)
      self$data$hash = digest::digest(c(data$task[[1L]]$hash, data$learner[[1L]]$hash, data$resampling[[1L]]$hash), algo = "xxhash64")
    },

    print = function(...) {
      catf("ResampleResult of learner '%s' on task '%s' with %i iterations", self$task$id, self$learner$id, nrow(self$data))
      # vapply(self$performance[, !"iteration"], function(x) c(mean(x), sd(x)))
      # ... TBC
    },

    experiment = function(iter) {
      iter = assert_int(iter, lower = 1L, upper = nrow(self$data), coerce = TRUE)
      .mapply(Experiment$new, self$data[get("iteration") == iter], MoreArgs = list())[[1L]]
    },

    experiments = function(iters) {
      iters = assert_integerish(iters, lower = 1L, upper = nrow(self$data), any.missing = FALSE, coerce = TRUE)
      .mapply(Experiment$new, self$data[get("iteration") %in% iters], MoreArgs = list())
    }

  ),

  active = list(
    task = function() {
      self$data$task[[1L]]
    },

    learner = function() {
      self$data$learner[[1L]]
    },

    resampling = function() {
      self$data$resampling[[1L]]
    },

    measures = function() {
      self$data$task[[1L]]$measures
    },

    performance = function() {
      perf = rbindlist(self$data$performance, fill = TRUE)
      cbind(data.table(iteration = seq_row(self$data)), perf)
    },

    aggregated = function() {
      measures = self$data$task[[1L]]$measures
      setNames(vnapply(measures, function(m) m$aggregate(self)), ids(measures))
    }
  )
)
