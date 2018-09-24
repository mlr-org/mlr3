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
#' rr$combine(rr)
#' rr$performance
#' rr$aggregated
#' rr$hash
#' ```
#'
#' @section Arguments:
#' * `i` (`integer`):
#'   Iteration(s) of the experiment(s) to retrieve.
#' * `rr` (`ResampleResult`):
#'   Second [ResampleResult].
#'
#' @section Details:
#' `$task`, `learner`, `resampling` and `measure` allow access to the [Task], [Learner], [Resampling] and [Measure] used in
#' the resampling.
#'
#' `$experiment()` returns an [Experiment] for the `iter`-th resampling iteration.
#'
#' `$experiments()` returns a list with the slice of [Experiment]s for the provided `iters`.
#'
#' `$combine()` takes a second [ResampleResult] and combines both [ResampleResult]s into a [BenchmarkResult].
#'
#' `$performance` provides a [data.table::data.table] with column `iteration` (integer) and a numeric column for each
#'   performance measure (columns named using the measure ids).
#'
#' `$aggregated` returns the aggregated performance measures. The aggregation method is part of the [Measure].
#'
#' `$hash` stores a hash for the combination of task, learner and resampling.
#'
#' @name ResampleResult
NULL

ResampleResult = R6Class("ResampleResult",
  cloneable = FALSE,
  public = list(
    data = NULL,

    initialize = function(data, hash = NULL) {
      assert_data_table(data)
      slots = reflections$experiment_slots$name
      assert_names(names(data), must.include = slots)
      self$data = data[, slots, with = FALSE]
      if (!is.null(hash))
        private$.hash = assert_string(hash)
    },

    print = function(...) {
      catf("ResampleResult of learner '%s' on task '%s' with %i iterations", self$task$id, self$learner$id, nrow(self$data))

      # FIXME: We want something like skimr w/o the dependencies
      perf = self$performance[, !"iteration"]
      tab = rbindlist(lapply(perf, function(x) c(as.list(summary(x)), list(Sd = sd(x)))))
      tab$Measure = names(perf)
      setcolorder(tab, "Measure")
      print(tab, class = FALSE, row.names = FALSE, print.keys = FALSE)
    },

    experiment = function(iter) {
      iter = assert_int(iter, lower = 1L, upper = nrow(self$data), coerce = TRUE)
      cols = reflections$experiment_slots$name
      .mapply(Experiment$new, self$data[get("iteration") == iter, cols, with = FALSE], MoreArgs = list())[[1L]]
    },

    experiments = function(iters) {
      iters = assert_integerish(iters, lower = 1L, upper = nrow(self$data), any.missing = FALSE, coerce = TRUE)
      cols = reflections$experiment_slots$name
      .mapply(Experiment$new, self$data[get("iteration") %in% iters, cols, with = FALSE], MoreArgs = list())
    },

    combine = function(rr) {
      assert_resample_result(rr)
      if (self$hash == rr$hash)
        warningf("ResampleResult$combine(): Identical hashes detected. This is likely to be unintended.")
      BenchmarkResult$new(rbind(cbind(self$data, data.table(hash = self$hash)), cbind(rr$data, data.table(hash = rr$hash))))
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
    },

    hash = function() {
      if (is.na(private$.hash)) {
        data = self$data
        private$.hash = hash_experiment(data$task[[1L]], data$learner[[1L]], data$resampling[[1L]])
      }
      private$.hash
    }
  ),

  private = list(
    .hash = NA_character_
  )
)
