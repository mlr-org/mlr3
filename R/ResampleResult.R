#' @title Container for Results of `resample()`
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [resample()].
#'
#' Note that all stored objects are accessed by reference.
#' Do not modify any object without cloning it first.
#'
#' @section Construction:
#' ```
#' rr = ResampleResult$new(data)
#' ```
#'
#' * `data` :: [data.table::data.table()]\cr
#'   Table with the data of one [Experiment] per row.
#'   See description of field `data` of [Experiment] for the exact structure.
#'
#' @section Fields:
#' * `data` :: [data.table::data.table()]\cr
#'   Experiment data with one [Experiment] per line.
#'
#' * `task` :: [Task]\cr
#'   The task [resample()] operated on.
#'
#' * `learner` :: [Learner]\cr
#'   The learner [resample()] operated on.
#'
#' * `resampling` :: [Resampling]\cr
#'   The resampling splits [resample()] operated on.
#'
#' * `measures` :: `list()` of [Measure]\cr
#'   The performance measures [resample()] operated on.
#'
#' * `errors` :: `logical()`\cr
#'   Logical vector where the i-th element is `TRUE` if an error for the i-th resampling iteration has been captured.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) for this object.
#'
#' * `aggregated` :: named `numeric()`\cr
#'   Returns a single score for each measure, named with measure ids.
#'
#' * `prediction` :: [Prediction]\cr
#'   Combined [Prediction] of all individual experiments.
#'   Note that the performance is not calculated on this object,
#'   but instead on each experiment separately and then combined with an aggregate function.
#'
#' @section Methods:
#' * `combine(rr)`\cr
#'   [ResampleResult] -> [BenchmarkResult]\cr
#'   Takes a second [ResampleResult] and combines both [ResampleResult]s to a [BenchmarkResult].
#'
#' * `experiment(iter)`\cr
#'   `integer(1)` -> [Experiment]\cr
#'   Returns the `iter`-th [Experiment].
#'
#' * `experiments(iters)`\cr
#'   `integer()` -> `list()` of [Experiment].
#'   Returns a slice of [Experiment]s with provided resampling iterations `iters`.
#'   Defaults to all experiments.
#'
#' * `performance(id)`\cr
#'   `character(1)` -> `numeric(1)`\cr
#'   Retrieves the performance values for the measure with id `id` as numeric vector.
#'
#' @section S3 Methods:
#' * `as.data.table(rr)`\cr
#'   [ResampleResult] -> [data.table::data.table()]\cr
#'   Converts the data to a `data.table()`.
#' @export
ResampleResult = R6Class("ResampleResult",
  public = list(
    data = NULL,

    initialize = function(data, hash = NULL) {
      assert_data_table(data)
      assert_names(names(data), must.include = mlr_reflections$experiment_slots$name)
      self$data = data[order(iteration), ]
      if (!is.null(hash)) {
        private$.hash = assert_string(hash)
      }
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function(digits = 4L, ...) {
      catf("%s of learner '%s' on task '%s' with %i iterations", format(self), self$task$id, self$learner$id, nrow(self$data))

      tab = map_dtr(self$measures$measure_id, function(id) {
        perf = self$performance(id)
        c(list(Measure = id), as.list(summary(perf)), list(Sd = sd(perf)))
      })
      print(tab, class = FALSE, row.names = FALSE, print.keys = FALSE, digits = digits, ...)
    },

    performance = function(id) {
      assert_choice(id, self$measures$measure_id)
      map_dbl(self$data$performance, function(x) x[[id]] %??% NA_real_)
    },

    experiment = function(iter) {
      iter = assert_int(iter, lower = 1L, upper = nrow(self$data), coerce = TRUE)
      pmap(self$data[get("iteration") == iter, mlr_reflections$experiment_slots$name, with = FALSE], as_experiment)[[1L]]
    },

    experiments = function(iters = NULL) {
      iters = if (is.null(iters)) {
        seq_row(self$data)
      } else {
        assert_integerish(iters, lower = 1L, upper = nrow(self$data), any.missing = FALSE, coerce = TRUE)
      }
      pmap(self$data[get("iteration") %in% iters], as_experiment)
    },

    combine = function(rr) {
      assert_resample_result(rr)
      if (self$hash == rr$hash) {
        warningf("ResampleResult$combine(): Identical hashes detected. This is likely to be unintended.")
      }
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
      set_names(map_dbl(measures, function(m) m$aggregate(self)), ids(measures))
    },

    prediction = function() {
      do.call(rbind, map(self$experiments(), "prediction"))
    },

    hash = function() {
      if (is.na(private$.hash)) {
        private$.hash = self$experiment(1L)$hash
      }
      private$.hash
    },

    errors = function() {
      has_error = function(log) !is.null(log) && log[get("class") == "error", .N] > 0L
      map_lgl(self$data$train_log, has_error)
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
