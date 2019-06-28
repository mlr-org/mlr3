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
#'   Table with the data of one resampling iteration per row.
#'
#' @section Fields:
#' * `data` :: [data.table::data.table()]\cr
#'   Internal data storage.
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
#' * `errors` :: `logical()`\cr
#'   Logical vector where the i-th element is `TRUE` if an error for the i-th resampling iteration has been captured.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) for this object.
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
#' * `performance(measures)`\cr
#'   `list()` of [Measure] -> `data.table()`\cr
#'   Retrieves the performance values for measures as [data.table()].
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
      self$data = assert_data_table(data)
      assert_names(names(data), must.include = mlr_reflections$rr_names)
      if (!is.null(hash)) {
        private$.hash = assert_string(hash)
      }
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf("%s of %i iterations", format(self), nrow(self$data))
      catf(str_indent("Task:", self$task$id))
      catf(str_indent("Learner:", self$data$learner[[1L]]$id))
    },

    combine = function(rr) {
      assert_resample_result(rr)
      if (self$hash == rr$hash) {
        warningf("ResampleResult$combine(): Identical hashes detected. This is likely to be unintended.")
      }
      BenchmarkResult$new(rbind(cbind(self$data, data.table(hash = self$hash)), cbind(rr$data, data.table(hash = rr$hash))))
    },

    performance = function(measures) {
      measures = assert_measures(measures)
      f = function(prediction, task, learner) as.list(prediction$score(measures, task = task, learner = learner))
      pmap_dtr(self$data[, c("prediction", "task", "learner"), with = FALSE], f)
    },

    aggregate = function(measures) {
      measures = assert_measures(measures)
      set_names(map_dbl(measures, function(m) m$aggregate(self)), ids(measures))
    }
  ),

  active = list(
    task = function() {
      self$data$task[[1L]]
    },

    learners = function() {
      self$data$learner
    },

    resampling = function() {
      self$data$resampling[[1L]]
    },

    prediction = function() {
      do.call(c, self$data$prediction)
    },

    hash = function() {
      if (is.null(private$.hash)) {
        row = as.list(self$data[1L])
        private$.hash = hash(row$task$hash, row$learner$hash, row$resampling$hash)
      }
      private$.hash
    },

    errors = function() {
      has_error = function(log) !is.null(log) && log[get("class") == "error", .N] > 0L
      map_lgl(self$data$train_log, has_error)
    }
  ),

  private = list(
    .hash = NULL,
    deep_clone = function(name, value) {
      if (name == "data") copy(value) else value
    }
  )
)

#' @export
as.data.table.ResampleResult = function(x, ...) {
  task = learner = resampling = iteration = performance = NULL
  tab = x$data[order(iteration),
    list(
      hash = x$hash,
      task = task, task_id = ids(task),
      learner = learner, learner_id = ids(learner),
      resampling = resampling, resampling_id = ids(resampling),
      iteration = iteration, performance = performance
    )
  ]
  unnest(tab, "performance")
}
