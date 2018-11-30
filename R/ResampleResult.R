#' @title Results of Resampling
#'
#' @description
#' This is the object returned by [resample()].
#'
#' @section Usage:
#'
#' ```
#' # Construction
#' rr = ResampleResult$new(data, hash = NULL)
#' #
#' rr$task
#' rr$learner
#' rr$resampling
#' rr$measures
#' rr$performance(id)
#' rr$experiment(iter)
#' rr$experiments(iters)
#' rr$combine(rr)
#' rr$aggregated
#' rr$hash
#' as.data.table(bmr)
#' ```
#'
#' @section Arguments:
#' * `data` ([data.table::data.table()]):\cr
#'   `data.table` with columns matching the data of an [Experiment].
#'   Each row corresponds to a single experiment.
#' * `hash` (`NULL` | `character(1)`):\cr
#'   Pre-calculated hash for the combination of `task`, `learner` and `resampling`.
#'   If `NULL`, the checksum will be calculated on-demand.
#' * `id` (`character(1)`):\cr
#'   Identifier of a performance measure.
#' * `iter` (`integer(1)`):\cr
#'   Iteration of the experiment to retrieve.
#' * `iters` (`integer`):\cr
#'   Iterations of experiments to retrieve as `list()`.
#' * `rr` (`ResampleResult`):\cr
#'   Second [ResampleResult].
#'
#' @section Details:
#' * `$task`, `$learner`, `$resampling` and `$measure` allow access to the [Task], [Learner], [Resampling] and
#'   [Measure] used in the resampling.
#'
#' * `$performance(id)` retrieves the performance values for the measure with id `id` as numeric vector.
#'
#' * `$experiment()` returns an [Experiment] for the `iter`-th resampling iteration.
#'
#' * `$experiments()` returns a `list` with the slice of [Experiment]s for the provided `iters`.
#'
#' * `$combine()` takes a second [ResampleResult] and combines both [ResampleResult]s to a [BenchmarkResult].
#'
#' * `$aggregated` (named `numeric()`) returns the aggregated performance measures. The aggregation method is part of the [Measure].
#'
#' * `$hash` (`character(1))` stores a hash for the combination of task, learner and resampling.
#'
#' * `as.data.table()` converts the [BenchmarkResult] to a [data.table::data.table()].
#'
#' @name ResampleResult
NULL

ResampleResult = R6Class("ResampleResult",
  public = list(
    data = NULL,

    initialize = function(data, hash = NULL) {
      assert_data_table(data)
      assert_names(names(data), must.include = mlr_reflections$experiment_slots$name)
      self$data = data[order(iteration), ]
      if (!is.null(hash))
        private$.hash = assert_string(hash)
    },

    print = function(...) {
      catf("ResampleResult of learner '%s' on task '%s' with %i iterations", self$task$id, self$learner$id, nrow(self$data))

      tab = map_dtr(self$measures$measure_id, function(id) {
        perf = self$performance(id)
        c(list(Measure = id), as.list(summary(perf)), list(Sd = sd(perf)))
      })
      print(tab, class = FALSE, row.names = FALSE, print.keys = FALSE)
    },

    performance = function(id) {
      assert_choice(id, self$measures$measure_id)
      map_dbl(self$data$performance, function(x) x[[id]] %??% NA_real)
    },

    experiment = function(iter) {
      iter = assert_int(iter, lower = 1L, upper = nrow(self$data), coerce = TRUE)
      .mapply(Experiment$new, self$data[get("iteration") == iter, mlr_reflections$experiment_slots$name, with = FALSE], MoreArgs = list())[[1L]]
    },

    experiments = function(iters) {
      iters = assert_integerish(iters, lower = 1L, upper = nrow(self$data), any.missing = FALSE, coerce = TRUE)
      .mapply(Experiment$new, self$data[get("iteration") %in% iters], MoreArgs = list())
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
      unique(map_dtr(self$data$measures, function(m) data.table(measure_id = ids(m), measure = m)), by = "measure_id")
    },

    aggregated = function() {
      measures = self$measures$measure
      setNames(map_dbl(measures, function(m) m$aggregate(self)), ids(measures))
    },

    hash = function() {
      if (is.na(private$.hash))
        private$.hash = self$experiment(1L)$hash
      private$.hash
    }
  ),

  private = list(
    .hash = NA_character_,

    deep_clone = function(name, value) {
      if (name == "data") copy(value) else value
    }
  )
)

#' @export
as.data.frame.ResampleResult = function(x, ...) {
  setDF(as.data.table.ResampleResult(x))
}

#' @export
as.data.table.ResampleResult = function(x, ...) {
  task = learner = resampling = iteration = performance = NULL
  unnest(x$data[order(iteration),
    list(
      hash = x$hash,
      task = task, task_id = ids(task),
      learner = learner, learner_id = ids(learner),
      resampling = resampling, resampling_id = ids(resampling),
      iteration = iteration, performance = performance
    )
  ], "performance")
}
